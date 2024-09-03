//! Process window events.

use std::borrow::Cow;
use std::cmp::{min};
use std::env;
use std::f32;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::mem;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Instant};

use glutin::dpi::PhysicalSize;
use glutin::event::{ElementState, Event as GlutinEvent, ModifiersState, MouseButton, WindowEvent};
use glutin::event_loop::{ControlFlow, EventLoop, EventLoopProxy, EventLoopWindowTarget};
use glutin::platform::run_return::EventLoopExtRunReturn;
use log::info;
use serde_json as json;

use minterm_crossfont::{self, Size};

use minterm_terminal::config::LOG_TARGET_CONFIG;
use minterm_terminal::event::{Event as TerminalEvent, EventListener, Notify, OnResize};
use minterm_terminal::grid::{Dimensions, Scroll};
use minterm_terminal::index::{Column, Point, Side};
use minterm_terminal::sync::FairMutex;
use minterm_terminal::term::{SizeInfo, Term, TermMode};
use minterm_terminal::tty;

use crate::cli::Options as CLIOptions;
use crate::config::{self, Config};
use crate::daemon::start_daemon;
use crate::display::window::Window;
use crate::display::{self, Display, DisplayUpdate};
use crate::input::{self, ActionContext as _};
use crate::macos;
use crate::message_bar::{Message, MessageBuffer};
use crate::scheduler::{Scheduler};

/// Events dispatched through the UI event loop.
#[derive(Debug, Clone)]
pub enum Event {
    Terminal(TerminalEvent),
    DprChanged(f64, (u32, u32)),
    Scroll(Scroll),
    ConfigReload(PathBuf),
    Message(Message),
}

impl From<Event> for GlutinEvent<'_, Event> {
    fn from(event: Event) -> Self {
        GlutinEvent::UserEvent(event)
    }
}

impl From<TerminalEvent> for Event {
    fn from(event: TerminalEvent) -> Self {
        Event::Terminal(event)
    }
}

pub struct ActionContext<'a, N, T> {
    pub notifier: &'a mut N,
    pub terminal: &'a mut Term<T>,
    pub mouse: &'a mut Mouse,
    pub received_count: &'a mut usize,
    pub suppress_chars: &'a mut bool,
    pub modifiers: &'a mut ModifiersState,
    pub display: &'a mut Display,
    pub message_buffer: &'a mut MessageBuffer,
    pub display_update_pending: &'a mut DisplayUpdate,
    pub config: &'a mut Config,
    pub event_loop: &'a EventLoopWindowTarget<Event>,
    pub scheduler: &'a mut Scheduler,
    cli_options: &'a CLIOptions,
    font_size: &'a mut Size,
    dirty: &'a mut bool,
}

impl<'a, N: Notify + 'a, T: EventListener> input::ActionContext<T> for ActionContext<'a, N, T> {
    #[inline]
    fn write_to_pty<B: Into<Cow<'static, [u8]>>>(&self, val: B) {
        self.notifier.notify(val);
    }

    /// Request a redraw.
    #[inline]
    fn mark_dirty(&mut self) {
        *self.dirty = true;
    }

    #[inline]
    fn size_info(&self) -> SizeInfo {
        self.display.size_info
    }

    fn scroll(&mut self, scroll: Scroll) {
        self.terminal.scroll_display(scroll);
        *self.dirty = true;
    }

    #[inline]
    fn mouse_mode(&self) -> bool {
        self.terminal.mode().intersects(TermMode::MOUSE_MODE)
    }

    #[inline]
    fn mouse_mut(&mut self) -> &mut Mouse {
        self.mouse
    }

    #[inline]
    fn mouse(&self) -> &Mouse {
        self.mouse
    }

    #[inline]
    fn received_count(&mut self) -> &mut usize {
        &mut self.received_count
    }

    #[inline]
    fn suppress_chars(&mut self) -> &mut bool {
        &mut self.suppress_chars
    }

    #[inline]
    fn modifiers(&mut self) -> &mut ModifiersState {
        &mut self.modifiers
    }

    #[inline]
    fn window(&mut self) -> &mut Window {
        &mut self.display.window
    }

    #[inline]
    fn display(&mut self) -> &mut Display {
        &mut self.display
    }

    #[inline]
    fn terminal(&self) -> &Term<T> {
        self.terminal
    }

    #[inline]
    fn terminal_mut(&mut self) -> &mut Term<T> {
        self.terminal
    }

    fn spawn_new_instance(&mut self) {
        let mut env_args = env::args();
        let minterm = env_args.next().unwrap();

        #[cfg(unix)]
        let mut args = {
            // Use working directory of controlling process, or fallback to initial shell.
            let mut pid = unsafe { libc::tcgetpgrp(tty::master_fd()) };
            if pid < 0 {
                pid = tty::child_pid();
            }

            let cwd = macos::proc::cwd(pid);

            // Add the current working directory as parameter.
            cwd.map(|path| vec!["--working-directory".into(), path]).unwrap_or_default()
        };

        #[cfg(not(unix))]
        let mut args: Vec<PathBuf> = Vec::new();

        let working_directory_set = !args.is_empty();

        // Reuse the arguments passed to Minterm for the new instance.
        while let Some(arg) = env_args.next() {
            // Drop working directory from existing parameters.
            if working_directory_set && arg == "--working-directory" {
                let _ = env_args.next();
                continue;
            }

            args.push(arg.into());
        }

        start_daemon(&minterm, &args);
    }

    #[inline]
    fn pop_message(&mut self) {
        if !self.message_buffer.is_empty() {
            self.display_update_pending.dirty = true;
            self.message_buffer.pop();
            *self.dirty = true;
        }
    }

    /// Handle keyboard typing start.
    ///
    /// This will temporarily disable some features like mouse cursor.
    ///
    /// All features are re-enabled again automatically.
    #[inline]
    fn on_typing_start(&mut self) {
        // Hide mouse cursor.
        if self.config.ui_config.mouse.hide_when_typing {
            self.display.window.set_mouse_visible(false);
        }
    }

    fn message(&self) -> Option<&Message> {
        self.message_buffer.message()
    }

    fn config(&self) -> &Config {
        self.config
    }

    fn event_loop(&self) -> &EventLoopWindowTarget<Event> {
        self.event_loop
    }

    fn scheduler_mut(&mut self) -> &mut Scheduler {
        self.scheduler
    }
}

impl<'a, N: Notify + 'a, T: EventListener> ActionContext<'a, N, T> {
}

#[derive(Debug, Eq, PartialEq)]
pub enum ClickState {
    None,
    Click,
}

/// State of the mouse.
#[derive(Debug)]
pub struct Mouse {
    pub left_button_state: ElementState,
    pub middle_button_state: ElementState,
    pub right_button_state: ElementState,
    pub last_click_timestamp: Instant,
    pub last_click_button: MouseButton,
    pub click_state: ClickState,
    pub scroll_px: f64,
    pub cell_side: Side,
    pub lines_scrolled: f32,
    pub inside_text_area: bool,
    pub x: usize,
    pub y: usize,
}

impl Default for Mouse {
    fn default() -> Mouse {
        Mouse {
            last_click_timestamp: Instant::now(),
            last_click_button: MouseButton::Left,
            left_button_state: ElementState::Released,
            middle_button_state: ElementState::Released,
            right_button_state: ElementState::Released,
            click_state: ClickState::None,
            cell_side: Side::Left,
            inside_text_area: Default::default(),
            lines_scrolled: Default::default(),
            scroll_px: Default::default(),
            x: Default::default(),
            y: Default::default(),
        }
    }
}

impl Mouse {
    /// Convert mouse pixel coordinates to viewport point.
    ///
    /// If the coordinates are outside of the terminal grid, like positions inside the padding, the
    /// coordinates will be clamped to the closest grid coordinates.
    #[inline]
    pub fn point(&self, size: &SizeInfo, display_offset: usize) -> Point {
        let col = self.x.saturating_sub(size.padding_x() as usize) / (size.cell_width() as usize);
        let col = min(Column(col), size.last_column());

        let line = self.y.saturating_sub(size.padding_y() as usize) / (size.cell_height() as usize);
        let line = min(line, size.bottommost_line().0 as usize);

        display::viewport_to_point(display_offset, Point::new(line, col))
    }
}

/// The event processor.
///
/// Stores some state from received events and dispatches actions when they are
/// triggered.
pub struct Processor<N> {
    notifier: N,
    mouse: Mouse,
    received_count: usize,
    suppress_chars: bool,
    modifiers: ModifiersState,
    config: Config,
    message_buffer: MessageBuffer,
    display: Display,
    font_size: Size,
    event_queue: Vec<GlutinEvent<'static, Event>>,
    cli_options: CLIOptions,
    dirty: bool,
}

impl<N: Notify + OnResize> Processor<N> {
    /// Create a new event processor.
    ///
    /// Takes a writer which is expected to be hooked up to the write end of a PTY.
    pub fn new(
        notifier: N,
        message_buffer: MessageBuffer,
        config: Config,
        display: Display,
        cli_options: CLIOptions,
    ) -> Processor<N> {
        Processor {
            font_size: config.ui_config.font.size(),
            message_buffer,
            cli_options,
            notifier,
            display,
            config,
            received_count: Default::default(),
            suppress_chars: Default::default(),
            event_queue: Default::default(),
            modifiers: Default::default(),
            mouse: Default::default(),
            dirty: Default::default(),
        }
    }

    /// Return `true` if `event_queue` is empty, `false` otherwise.
    #[inline]
    fn event_queue_empty(&mut self) -> bool {
        self.event_queue.is_empty()
    }

    /// Run the event loop.
    pub fn run<T>(&mut self, terminal: Arc<FairMutex<Term<T>>>, mut event_loop: EventLoop<Event>)
    where
        T: EventListener,
    {
        let mut scheduler = Scheduler::new();

        event_loop.run_return(|event, event_loop, control_flow| {
            if self.config.ui_config.debug.print_events {
                info!("glutin event: {:?}", event);
            }

            // Ignore all events we do not care about.
            if Self::skip_event(&event) {
                return;
            }

            match event {
                // Check for shutdown.
                GlutinEvent::UserEvent(Event::Terminal(TerminalEvent::Exit)) => {
                    *control_flow = ControlFlow::Exit;
                    return;
                },
                // Process events.
                GlutinEvent::RedrawEventsCleared => {
                    *control_flow = match scheduler.update(&mut self.event_queue) {
                        Some(instant) => ControlFlow::WaitUntil(instant),
                        None => ControlFlow::Wait,
                    };

                    if self.event_queue_empty() {
                        return;
                    }
                },
                // Remap DPR change event to remove lifetime.
                GlutinEvent::WindowEvent {
                    event: WindowEvent::ScaleFactorChanged { scale_factor, new_inner_size },
                    ..
                } => {
                    *control_flow = ControlFlow::Poll;
                    let size = (new_inner_size.width, new_inner_size.height);
                    self.event_queue.push(Event::DprChanged(scale_factor, size).into());
                    return;
                },
                // Transmute to extend lifetime, which exists only for `ScaleFactorChanged` event.
                // Since we remap that event to remove the lifetime, this is safe.
                event => unsafe {
                    *control_flow = ControlFlow::Poll;
                    self.event_queue.push(mem::transmute(event));
                    return;
                },
            }

            let mut terminal = terminal.lock();

            let mut display_update_pending = DisplayUpdate::default();

            let context = ActionContext {
                terminal: &mut terminal,
                notifier: &mut self.notifier,
                mouse: &mut self.mouse,
                received_count: &mut self.received_count,
                suppress_chars: &mut self.suppress_chars,
                modifiers: &mut self.modifiers,
                message_buffer: &mut self.message_buffer,
                display_update_pending: &mut display_update_pending,
                display: &mut self.display,
                font_size: &mut self.font_size,
                config: &mut self.config,
                scheduler: &mut scheduler,
                cli_options: &self.cli_options,
                dirty: &mut self.dirty,
                event_loop,
            };
            let mut processor = input::Processor::new(context);

            for event in self.event_queue.drain(..) {
                Processor::handle_event(event, &mut processor);
            }

            // Process DisplayUpdate events.
            if display_update_pending.dirty {
                self.submit_display_update(&mut terminal, display_update_pending);
            }

            if self.dirty {
                self.dirty = false;

                // Redraw screen.
                self.display.draw(terminal, &self.message_buffer, &self.config);
            }
        });

        // Write ref tests to disk.
        if self.config.ui_config.debug.ref_test {
            self.write_ref_test_results(&terminal.lock());
        }
    }

    /// Handle events from glutin.
    ///
    /// Doesn't take self mutably due to borrow checking.
    fn handle_event<T>(
        event: GlutinEvent<'_, Event>,
        processor: &mut input::Processor<T, ActionContext<'_, N, T>>,
    ) where
        T: EventListener,
    {
        match event {
            GlutinEvent::UserEvent(event) => match event {
                Event::DprChanged(scale_factor, (width, height)) => {
                    let display_update_pending = &mut processor.ctx.display_update_pending;

                    // Push current font to update its DPR.
                    let font = processor.ctx.config.ui_config.font.clone();
                    display_update_pending.set_font(font.with_size(*processor.ctx.font_size));

                    // Resize to event's dimensions.
                    display_update_pending.set_dimensions(PhysicalSize::new(width, height));

                    processor.ctx.window().dpr = scale_factor;
                    *processor.ctx.dirty = true;
                },
                Event::Message(message) => {
                    processor.ctx.message_buffer.push(message);
                    processor.ctx.display_update_pending.dirty = true;
                    *processor.ctx.dirty = true;
                },
                Event::ConfigReload(path) => Self::reload_config(&path, processor),
                Event::Scroll(scroll) => processor.ctx.scroll(scroll),
                Event::Terminal(event) => match event {
                    TerminalEvent::Wakeup => *processor.ctx.dirty = true,
                    TerminalEvent::ColorRequest(index, format) => {
                        let text = format(processor.ctx.display.colors[index]);
                        processor.ctx.write_to_pty(text.into_bytes());
                    },
                    TerminalEvent::PtyWrite(text) => processor.ctx.write_to_pty(text.into_bytes()),
                    TerminalEvent::MouseCursorDirty => processor.reset_mouse_cursor(),
                    TerminalEvent::Exit => (),
                },
            },
            GlutinEvent::RedrawRequested(_) => *processor.ctx.dirty = true,
            GlutinEvent::WindowEvent { event, window_id, .. } => {
                match event {
                    WindowEvent::CloseRequested => processor.ctx.terminal.exit(),
                    WindowEvent::Resized(size) => {
                        // Minimizing the window sends a Resize event with zero width and
                        // height. But there's no need to ever actually resize to this.

                        processor.ctx.display_update_pending.set_dimensions(size);
                        *processor.ctx.dirty = true;
                    },
                    WindowEvent::KeyboardInput { input, is_synthetic: false, .. } => {
                        processor.key_input(input);
                    },
                    WindowEvent::ModifiersChanged(modifiers) => {
                        processor.modifiers_input(modifiers)
                    },
                    WindowEvent::ReceivedCharacter(c) => processor.received_char(c),
                    WindowEvent::MouseInput { state, button, .. } => {
                        processor.ctx.window().set_mouse_visible(true);
                        processor.mouse_input(state, button);
                        *processor.ctx.dirty = true;
                    },
                    WindowEvent::CursorMoved { position, .. } => {
                        processor.ctx.window().set_mouse_visible(true);
                        processor.mouse_moved(position);
                    },
                    WindowEvent::MouseWheel { delta, phase, .. } => {
                        processor.ctx.window().set_mouse_visible(true);
                        processor.mouse_wheel_input(delta, phase);
                    },
                    WindowEvent::Focused(is_focused) => {
                        if window_id == processor.ctx.window().window_id() {
                            processor.ctx.terminal.is_focused = is_focused;
                            *processor.ctx.dirty = true;

                            if is_focused {
                                processor.ctx.window().set_urgent(false);
                            } else {
                                processor.ctx.window().set_mouse_visible(true);
                            }

                            processor.on_focus_change(is_focused);
                        }
                    },
                    WindowEvent::DroppedFile(path) => {
                        let path: String = path.to_string_lossy().into();
                        processor.ctx.write_to_pty((path + " ").into_bytes());
                    },
                    WindowEvent::CursorLeft { .. } => {
                        processor.ctx.mouse.inside_text_area = false;

                    },
                    WindowEvent::KeyboardInput { is_synthetic: true, .. }
                    | WindowEvent::TouchpadPressure { .. }
                    | WindowEvent::ScaleFactorChanged { .. }
                    | WindowEvent::CursorEntered { .. }
                    | WindowEvent::AxisMotion { .. }
                    | WindowEvent::HoveredFileCancelled
                    | WindowEvent::Destroyed
                    | WindowEvent::ThemeChanged(_)
                    | WindowEvent::HoveredFile(_)
                    | WindowEvent::Touch(_)
                    | WindowEvent::Moved(_) => (),
                }
            },
            GlutinEvent::Suspended { .. }
            | GlutinEvent::NewEvents { .. }
            | GlutinEvent::DeviceEvent { .. }
            | GlutinEvent::MainEventsCleared
            | GlutinEvent::RedrawEventsCleared
            | GlutinEvent::Resumed
            | GlutinEvent::LoopDestroyed => (),
        }
    }

    /// Check if an event is irrelevant and can be skipped.
    fn skip_event(event: &GlutinEvent<'_, Event>) -> bool {
        match event {
            GlutinEvent::WindowEvent { event, .. } => matches!(
                event,
                WindowEvent::KeyboardInput { is_synthetic: true, .. }
                    | WindowEvent::TouchpadPressure { .. }
                    | WindowEvent::CursorEntered { .. }
                    | WindowEvent::AxisMotion { .. }
                    | WindowEvent::HoveredFileCancelled
                    | WindowEvent::Destroyed
                    | WindowEvent::HoveredFile(_)
                    | WindowEvent::Touch(_)
                    | WindowEvent::Moved(_)
            ),
            GlutinEvent::Suspended { .. }
            | GlutinEvent::NewEvents { .. }
            | GlutinEvent::MainEventsCleared
            | GlutinEvent::LoopDestroyed => true,
            _ => false,
        }
    }

    /// Reload the configuration files from disk.
    fn reload_config<T>(path: &Path, processor: &mut input::Processor<T, ActionContext<'_, N, T>>)
    where
        T: EventListener,
    {
        if !processor.ctx.message_buffer.is_empty() {
            processor.ctx.message_buffer.remove_target(LOG_TARGET_CONFIG);
            processor.ctx.display_update_pending.dirty = true;
        }

        let config = match config::reload(path, processor.ctx.cli_options) {
            Ok(config) => config,
            Err(_) => return,
        };

        processor.ctx.display.update_config(&config);
        processor.ctx.terminal.update_config(&config);

        // Reload cursor if its thickness has changed.
        if (processor.ctx.config.cursor.thickness() - config.cursor.thickness()).abs()
            > f32::EPSILON
        {
            processor.ctx.display_update_pending.set_cursor_dirty();
        }

        if processor.ctx.config.ui_config.font != config.ui_config.font {
            // Do not update font size if it has been changed at runtime.
            if *processor.ctx.font_size == processor.ctx.config.ui_config.font.size() {
                *processor.ctx.font_size = config.ui_config.font.size();
            }

            let font = config.ui_config.font.clone().with_size(*processor.ctx.font_size);
            processor.ctx.display_update_pending.set_font(font);
        }

        // Update display if padding options were changed.
        let window_config = &processor.ctx.config.ui_config.window;
        if window_config.padding(1.) != config.ui_config.window.padding(1.)
            || window_config.dynamic_padding != config.ui_config.window.dynamic_padding
        {
            processor.ctx.display_update_pending.dirty = true;
        }

        // Set subpixel anti-aliasing.
        minterm_crossfont::set_font_smoothing(config.ui_config.font.use_thin_strokes);

        // Disable shadows
        processor.ctx.window().set_has_shadow(false);

        *processor.ctx.config = config;

        *processor.ctx.dirty = true;
    }

    /// Submit the pending changes to the `Display`.
    fn submit_display_update<T>(
        &mut self,
        terminal: &mut Term<T>,
        display_update_pending: DisplayUpdate,
    ) where
        T: EventListener,
    {
        self.display.handle_update(
            terminal,
            &mut self.notifier,
            &self.config,
            display_update_pending,
        );
    }

    /// Write the ref test results to the disk.
    fn write_ref_test_results<T>(&self, terminal: &Term<T>) {
        // Dump grid state.
        let mut grid = terminal.grid().clone();
        grid.initialize_all();
        grid.truncate();

        let serialized_grid = json::to_string(&grid).expect("serialize grid");

        let serialized_size = json::to_string(&self.display.size_info).expect("serialize size");

        let serialized_config = format!("{{\"history_size\":{}}}", grid.history_size());

        File::create("./grid.json")
            .and_then(|mut f| f.write_all(serialized_grid.as_bytes()))
            .expect("write grid.json");

        File::create("./size.json")
            .and_then(|mut f| f.write_all(serialized_size.as_bytes()))
            .expect("write size.json");

        File::create("./config.json")
            .and_then(|mut f| f.write_all(serialized_config.as_bytes()))
            .expect("write config.json");
    }
}

#[derive(Debug, Clone)]
pub struct EventProxy(EventLoopProxy<Event>);

impl EventProxy {
    pub fn new(proxy: EventLoopProxy<Event>) -> Self {
        EventProxy(proxy)
    }

    /// Send an event to the event loop.
    pub fn send_event(&self, event: Event) {
        let _ = self.0.send_event(event);
    }
}

impl EventListener for EventProxy {
    fn send_event(&self, event: TerminalEvent) {
        let _ = self.0.send_event(Event::Terminal(event));
    }
}
