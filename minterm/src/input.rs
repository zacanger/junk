//! Handle input from glutin.
//!
//! Certain key combinations should send some escape sequence back to the PTY.
//! In order to figure that out, state about which modifier keys are pressed
//! needs to be tracked. Additionally, we need a bit of a state machine to
//! determine what to do when a non-modifier key is pressed.

use std::borrow::Cow;
use std::cmp::{max, min, Ordering};
use std::marker::PhantomData;
use std::time::{Instant};

use glutin::dpi::PhysicalPosition;
use glutin::event::{
    ElementState, KeyboardInput, ModifiersState, MouseButton, MouseScrollDelta, TouchPhase,
};
use glutin::event_loop::EventLoopWindowTarget;
use glutin::window::CursorIcon;

use minterm_terminal::event::EventListener;
use minterm_terminal::grid::{Dimensions, Scroll};
use minterm_terminal::index::{Column, Point, Side};
use minterm_terminal::term::{SizeInfo, Term, TermMode};

use crate::config::{Action, BindingMode, Config, Key};
use crate::daemon::start_daemon;
use crate::display::window::Window;
use crate::display::Display;
use crate::event::{ClickState, Event, Mouse};
use crate::message_bar::{self, Message};
use crate::scheduler::{Scheduler};

/// Processes input from glutin.
///
/// An escape sequence may be emitted in case specific keys or key combinations
/// are activated.
pub struct Processor<T: EventListener, A: ActionContext<T>> {
    pub ctx: A,
    _phantom: PhantomData<T>,
}

pub trait ActionContext<T: EventListener> {
    fn write_to_pty<B: Into<Cow<'static, [u8]>>>(&self, _data: B) {}
    fn mark_dirty(&mut self) {}
    fn size_info(&self) -> SizeInfo;
    fn mouse_mut(&mut self) -> &mut Mouse;
    fn mouse(&self) -> &Mouse;
    fn received_count(&mut self) -> &mut usize;
    fn suppress_chars(&mut self) -> &mut bool;
    fn modifiers(&mut self) -> &mut ModifiersState;
    fn scroll(&mut self, _scroll: Scroll) {}
    fn window(&mut self) -> &mut Window;
    fn display(&mut self) -> &mut Display;
    fn terminal(&self) -> &Term<T>;
    fn terminal_mut(&mut self) -> &mut Term<T>;
    fn spawn_new_instance(&mut self) {}
    fn pop_message(&mut self) {}
    fn message(&self) -> Option<&Message>;
    fn config(&self) -> &Config;
    fn event_loop(&self) -> &EventLoopWindowTarget<Event>;
    fn mouse_mode(&self) -> bool;
    fn scheduler_mut(&mut self) -> &mut Scheduler;
    fn on_typing_start(&mut self) {}
}

impl Action {
}

trait Execute<T: EventListener> {
    fn execute<A: ActionContext<T>>(&self, ctx: &mut A);
}

impl<T: EventListener> Execute<T> for Action {
    #[inline]
    fn execute<A: ActionContext<T>>(&self, ctx: &mut A) {
        match self {
            Action::Esc(s) => {
                ctx.on_typing_start();

                ctx.scroll(Scroll::Bottom);
                ctx.write_to_pty(s.clone().into_bytes())
            },
            Action::Command(program) => start_daemon(program.program(), program.args()),
            Action::ClearLogNotice => ctx.pop_message(),
            Action::ReceiveChar | Action::None => (),
        }
    }
}

impl<T: EventListener, A: ActionContext<T>> Processor<T, A> {
    pub fn new(ctx: A) -> Self {
        Self { ctx, _phantom: Default::default() }
    }

    #[inline]
    pub fn mouse_moved(&mut self, position: PhysicalPosition<f64>) {
        let size_info = self.ctx.size_info();

        let (x, y) = position.into();

        let lmb_pressed = self.ctx.mouse().left_button_state == ElementState::Pressed;

        let display_offset = self.ctx.terminal().grid().display_offset();
        let old_point = self.ctx.mouse().point(&size_info, display_offset);

        let x = min(max(x, 0), size_info.width() as i32 - 1) as usize;
        let y = min(max(y, 0), size_info.height() as i32 - 1) as usize;
        self.ctx.mouse_mut().x = x;
        self.ctx.mouse_mut().y = y;

        let inside_text_area = size_info.contains_point(x, y);
        let cell_side = self.cell_side(x);

        let point = self.ctx.mouse().point(&size_info, display_offset);
        let cell_changed = old_point != point;

        // If the mouse hasn't changed cells, do nothing.
        if !cell_changed
            && self.ctx.mouse().cell_side == cell_side
            && self.ctx.mouse().inside_text_area == inside_text_area
        {
            return;
        }

        self.ctx.mouse_mut().inside_text_area = inside_text_area;
        self.ctx.mouse_mut().cell_side = cell_side;

        // Update mouse state and check for URL change.
        let mouse_state = self.cursor_state();
        self.ctx.window().set_mouse_cursor(mouse_state);

        if cell_changed
            && self.ctx.terminal().mode().intersects(TermMode::MOUSE_MOTION | TermMode::MOUSE_DRAG)
        {
            if lmb_pressed {
                self.mouse_report(32, ElementState::Pressed);
            } else if self.ctx.mouse().middle_button_state == ElementState::Pressed {
                self.mouse_report(33, ElementState::Pressed);
            } else if self.ctx.mouse().right_button_state == ElementState::Pressed {
                self.mouse_report(34, ElementState::Pressed);
            } else if self.ctx.terminal().mode().contains(TermMode::MOUSE_MOTION) {
                self.mouse_report(35, ElementState::Pressed);
            }
        }
    }

    /// Check which side of a cell an X coordinate lies on.
    fn cell_side(&self, x: usize) -> Side {
        let size_info = self.ctx.size_info();

        let cell_x =
            x.saturating_sub(size_info.padding_x() as usize) % size_info.cell_width() as usize;
        let half_cell_width = (size_info.cell_width() / 2.0) as usize;

        let additional_padding =
            (size_info.width() - size_info.padding_x() * 2.) % size_info.cell_width();
        let end_of_grid = size_info.width() - size_info.padding_x() - additional_padding;

        if cell_x > half_cell_width
            // Edge case when mouse leaves the window.
            || x as f32 >= end_of_grid
        {
            Side::Right
        } else {
            Side::Left
        }
    }

    fn mouse_report(&mut self, button: u8, state: ElementState) {
        let display_offset = self.ctx.terminal().grid().display_offset();
        let point = self.ctx.mouse().point(&self.ctx.size_info(), display_offset);

        // Assure the mouse point is not in the scrollback.
        if point.line < 0 {
            return;
        }

        // Calculate modifiers value.
        let mut mods = 0;
        let modifiers = self.ctx.modifiers();
        if modifiers.shift() {
            mods += 4;
        }
        if modifiers.alt() {
            mods += 8;
        }
        if modifiers.ctrl() {
            mods += 16;
        }

        // Report mouse events.
        if self.ctx.terminal().mode().contains(TermMode::SGR_MOUSE) {
            self.sgr_mouse_report(point, button + mods, state);
        } else if let ElementState::Released = state {
            self.normal_mouse_report(point, 3 + mods);
        } else {
            self.normal_mouse_report(point, button + mods);
        }
    }

    fn normal_mouse_report(&mut self, point: Point, button: u8) {
        let Point { line, column } = point;
        let utf8 = self.ctx.terminal().mode().contains(TermMode::UTF8_MOUSE);

        let max_point = if utf8 { 2015 } else { 223 };

        if line >= max_point || column >= max_point {
            return;
        }

        let mut msg = vec![b'\x1b', b'[', b'M', 32 + button];

        let mouse_pos_encode = |pos: usize| -> Vec<u8> {
            let pos = 32 + 1 + pos;
            let first = 0xC0 + pos / 64;
            let second = 0x80 + (pos & 63);
            vec![first as u8, second as u8]
        };

        if utf8 && column >= Column(95) {
            msg.append(&mut mouse_pos_encode(column.0));
        } else {
            msg.push(32 + 1 + column.0 as u8);
        }

        if utf8 && line >= 95 {
            msg.append(&mut mouse_pos_encode(line.0 as usize));
        } else {
            msg.push(32 + 1 + line.0 as u8);
        }

        self.ctx.write_to_pty(msg);
    }

    fn sgr_mouse_report(&mut self, point: Point, button: u8, state: ElementState) {
        let c = match state {
            ElementState::Pressed => 'M',
            ElementState::Released => 'm',
        };

        let msg = format!("\x1b[<{};{};{}{}", button, point.column + 1, point.line + 1, c);
        self.ctx.write_to_pty(msg.into_bytes());
    }

    fn on_mouse_press(&mut self, button: MouseButton) {
        // Handle mouse mode.
        if !self.ctx.modifiers().shift() && self.ctx.mouse_mode() {
            self.ctx.mouse_mut().click_state = ClickState::None;

            let code = match button {
                MouseButton::Left => 0,
                MouseButton::Middle => 1,
                MouseButton::Right => 2,
                // Can't properly report more than three buttons..
                MouseButton::Other(_) => return,
            };

            self.mouse_report(code, ElementState::Pressed);
        } else {
            // Calculate time since the last click to handle clicks
            let now = Instant::now();
            self.ctx.mouse_mut().last_click_timestamp = now;

            // Update multi-click state.
            self.ctx.mouse_mut().click_state = match self.ctx.mouse().click_state {
                // Reset click state if button has changed.
                _ if button != self.ctx.mouse().last_click_button => {
                    self.ctx.mouse_mut().last_click_button = button;
                    ClickState::Click
                },
                _ => ClickState::Click,
            };
        }
    }

    fn on_mouse_release(&mut self, button: MouseButton) {
        if !self.ctx.modifiers().shift() && self.ctx.mouse_mode() {
            let code = match button {
                MouseButton::Left => 0,
                MouseButton::Middle => 1,
                MouseButton::Right => 2,
                // Can't properly report more than three buttons.
                MouseButton::Other(_) => return,
            };
            self.mouse_report(code, ElementState::Released);
            return;
        }
    }

    pub fn mouse_wheel_input(&mut self, delta: MouseScrollDelta, phase: TouchPhase) {
        match delta {
            MouseScrollDelta::LineDelta(_columns, lines) => {
                let new_scroll_px = lines * self.ctx.size_info().cell_height();
                self.scroll_terminal(f64::from(new_scroll_px));
            },
            MouseScrollDelta::PixelDelta(lpos) => {
                match phase {
                    TouchPhase::Started => {
                        // Reset offset to zero.
                        self.ctx.mouse_mut().scroll_px = 0.;
                    },
                    TouchPhase::Moved => {
                        self.scroll_terminal(lpos.y);
                    },
                    _ => (),
                }
            },
        }
    }

    fn scroll_terminal(&mut self, new_scroll_px: f64) {
        let height = f64::from(self.ctx.size_info().cell_height());

        if self.ctx.mouse_mode() {
            self.ctx.mouse_mut().scroll_px += new_scroll_px;

            let code = if new_scroll_px > 0. { 64 } else { 65 };
            let lines = (self.ctx.mouse().scroll_px / height).abs() as i32;

            for _ in 0..lines {
                self.mouse_report(code, ElementState::Pressed);
            }
        } else if self
            .ctx
            .terminal()
            .mode()
            .contains(TermMode::ALT_SCREEN | TermMode::ALTERNATE_SCROLL)
            && !self.ctx.modifiers().shift()
        {
            let multiplier = f64::from(self.ctx.config().scrolling.multiplier);
            self.ctx.mouse_mut().scroll_px += new_scroll_px * multiplier;

            let cmd = if new_scroll_px > 0. { b'A' } else { b'B' };
            let lines = (self.ctx.mouse().scroll_px / height).abs() as i32;

            let mut content = Vec::with_capacity(lines as usize * 3);
            for _ in 0..lines {
                content.push(0x1b);
                content.push(b'O');
                content.push(cmd);
            }
            self.ctx.write_to_pty(content);
        } else {
            let multiplier = f64::from(self.ctx.config().scrolling.multiplier);
            self.ctx.mouse_mut().scroll_px += new_scroll_px * multiplier;

            let lines = self.ctx.mouse().scroll_px / height;

            self.ctx.scroll(Scroll::Delta(lines as i32));
        }

        self.ctx.mouse_mut().scroll_px %= height;
    }

    pub fn on_focus_change(&mut self, is_focused: bool) {
        if self.ctx.terminal().mode().contains(TermMode::FOCUS_IN_OUT) {
            let chr = if is_focused { "I" } else { "O" };

            let msg = format!("\x1b[{}", chr);
            self.ctx.write_to_pty(msg.into_bytes());
        }
    }

    pub fn mouse_input(&mut self, state: ElementState, button: MouseButton) {
        match button {
            MouseButton::Left => self.ctx.mouse_mut().left_button_state = state,
            MouseButton::Middle => self.ctx.mouse_mut().middle_button_state = state,
            MouseButton::Right => self.ctx.mouse_mut().right_button_state = state,
            _ => (),
        }

        // Skip normal mouse events if the message bar has been clicked.
        if self.message_bar_cursor_state() == Some(CursorIcon::Hand)
            && state == ElementState::Pressed
        {
            let size = self.ctx.size_info();

            let current_lines = self.ctx.message().map(|m| m.text(&size).len()).unwrap_or(0);

            self.ctx.pop_message();

            // Reset cursor when message bar height changed or all messages are gone.
            let new_lines = self.ctx.message().map(|m| m.text(&size).len()).unwrap_or(0);

            let new_icon = match current_lines.cmp(&new_lines) {
                Ordering::Less => CursorIcon::Default,
                Ordering::Equal => CursorIcon::Hand,
                Ordering::Greater => {
                    if self.ctx.mouse_mode() {
                        CursorIcon::Default
                    } else {
                        CursorIcon::Text
                    }
                },
            };

            self.ctx.window().set_mouse_cursor(new_icon);
        } else {
            match state {
                ElementState::Pressed => {
                    // Process mouse press before bindings to update the `click_state`.
                    self.on_mouse_press(button);
                    self.process_mouse_bindings(button);
                },
                ElementState::Released => self.on_mouse_release(button),
            }
        }
    }

    /// Process key input.
    pub fn key_input(&mut self, input: KeyboardInput) {
        match input.state {
            ElementState::Pressed => {
                *self.ctx.received_count() = 0;
                self.process_key_bindings(input);
            },
            ElementState::Released => *self.ctx.suppress_chars() = false,
        }
    }

    /// Modifier state change.
    pub fn modifiers_input(&mut self, modifiers: ModifiersState) {
        *self.ctx.modifiers() = modifiers;

        // Update mouse state and check for URL change.
        let mouse_state = self.cursor_state();
        self.ctx.window().set_mouse_cursor(mouse_state);
    }

    /// Reset mouse cursor based on modifier and terminal state.
    #[inline]
    pub fn reset_mouse_cursor(&mut self) {
        let mouse_state = self.cursor_state();
        self.ctx.window().set_mouse_cursor(mouse_state);
    }

    /// Process a received character.
    pub fn received_char(&mut self, c: char) {
        let suppress_chars = *self.ctx.suppress_chars();

        if suppress_chars {
            return;
        }

        self.ctx.on_typing_start();

        self.ctx.scroll(Scroll::Bottom);

        let utf8_len = c.len_utf8();
        let mut bytes = Vec::with_capacity(utf8_len);
        unsafe {
            bytes.set_len(utf8_len);
            c.encode_utf8(&mut bytes[..]);
        }

        if self.ctx.config().ui_config.alt_send_esc
            && *self.ctx.received_count() == 0
            && self.ctx.modifiers().alt()
            && utf8_len == 1
        {
            bytes.insert(0, b'\x1b');
        }

        self.ctx.write_to_pty(bytes);

        *self.ctx.received_count() += 1;
    }

    /// Attempt to find a binding and execute its action.
    ///
    /// The provided mode, mods, and key must match what is allowed by a binding
    /// for its action to be executed.
    fn process_key_bindings(&mut self, input: KeyboardInput) {
        let mode = BindingMode::new(self.ctx.terminal().mode());
        let mods = *self.ctx.modifiers();
        let mut suppress_chars = None;

        for i in 0..self.ctx.config().ui_config.key_bindings().len() {
            let binding = &self.ctx.config().ui_config.key_bindings()[i];

            let key = match (binding.trigger, input.virtual_keycode) {
                (Key::Scancode(_), _) => Key::Scancode(input.scancode),
                (_, Some(key)) => Key::Keycode(key),
                _ => continue,
            };

            if binding.is_triggered_by(mode, mods, &key) {
                // Pass through the key if any of the bindings has the `ReceiveChar` action.
                *suppress_chars.get_or_insert(true) &= binding.action != Action::ReceiveChar;

                // Binding was triggered; run the action.
                binding.action.clone().execute(&mut self.ctx);
            }
        }

        // Don't suppress char if no bindings were triggered.
        *self.ctx.suppress_chars() = suppress_chars.unwrap_or(false);
    }

    /// Attempt to find a binding and execute its action.
    ///
    /// The provided mode, mods, and key must match what is allowed by a binding
    /// for its action to be executed.
    fn process_mouse_bindings(&mut self, button: MouseButton) {
        let mode = BindingMode::new(self.ctx.terminal().mode());
        let mouse_mode = self.ctx.mouse_mode();
        let mods = *self.ctx.modifiers();

        for i in 0..self.ctx.config().ui_config.mouse_bindings().len() {
            let mut binding = self.ctx.config().ui_config.mouse_bindings()[i].clone();

            // Require shift for all modifiers when mouse mode is active.
            if mouse_mode {
                binding.mods |= ModifiersState::SHIFT;
            }

            if binding.is_triggered_by(mode, mods, &button) {
                binding.action.execute(&mut self.ctx);
            }
        }
    }

    /// Check mouse icon state in relation to the message bar.
    fn message_bar_cursor_state(&self) -> Option<CursorIcon> {
        // Calculate Y position of the end of the last terminal line.
        let size = self.ctx.size_info();
        let terminal_end = size.padding_y() as usize
            + size.cell_height() as usize * (size.screen_lines());

        let mouse = self.ctx.mouse();
        let display_offset = self.ctx.terminal().grid().display_offset();
        let point = self.ctx.mouse().point(&self.ctx.size_info(), display_offset);

        if self.ctx.message().is_none() || (mouse.y <= terminal_end) {
            None
        } else if mouse.y <= terminal_end + size.cell_height() as usize
            && point.column + message_bar::CLOSE_BUTTON_TEXT.len() >= size.columns()
        {
            Some(CursorIcon::Hand)
        } else {
            Some(CursorIcon::Default)
        }
    }

    /// Icon state of the cursor.
    fn cursor_state(&mut self) -> CursorIcon {
        if let Some(mouse_state) = self.message_bar_cursor_state() {
            mouse_state
        } else if !self.ctx.modifiers().shift() && self.ctx.mouse_mode() {
            CursorIcon::Default
        } else {
            CursorIcon::Text
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use glutin::event::{Event as GlutinEvent, VirtualKeyCode, WindowEvent};

    use minterm_terminal::event::Event as TerminalEvent;

    use crate::config::Binding;
    use crate::message_bar::MessageBuffer;

    const KEY: VirtualKeyCode = VirtualKeyCode::Key0;

    struct MockEventProxy;
    impl EventListener for MockEventProxy {}

    struct ActionContext<'a, T> {
        pub terminal: &'a mut Term<T>,
        pub size_info: &'a SizeInfo,
        pub mouse: &'a mut Mouse,
        pub message_buffer: &'a mut MessageBuffer,
        pub received_count: usize,
        pub suppress_chars: bool,
        pub modifiers: ModifiersState,
        config: &'a Config,
    }

    impl<'a, T: EventListener> super::ActionContext<T> for ActionContext<'a, T> {
        fn terminal(&self) -> &Term<T> {
            self.terminal
        }

        fn terminal_mut(&mut self) -> &mut Term<T> {
            &mut self.terminal
        }

        fn size_info(&self) -> SizeInfo {
            *self.size_info
        }

        fn scroll(&mut self, scroll: Scroll) {
            self.terminal.scroll_display(scroll);
        }

        fn mouse_mode(&self) -> bool {
            false
        }

        #[inline]
        fn mouse_mut(&mut self) -> &mut Mouse {
            self.mouse
        }

        #[inline]
        fn mouse(&self) -> &Mouse {
            self.mouse
        }

        fn received_count(&mut self) -> &mut usize {
            &mut self.received_count
        }

        fn suppress_chars(&mut self) -> &mut bool {
            &mut self.suppress_chars
        }

        fn modifiers(&mut self) -> &mut ModifiersState {
            &mut self.modifiers
        }

        fn window(&mut self) -> &mut Window {
            unimplemented!();
        }

        fn display(&mut self) -> &mut Display {
            unimplemented!();
        }

        fn pop_message(&mut self) {
            self.message_buffer.pop();
        }

        fn message(&self) -> Option<&Message> {
            self.message_buffer.message()
        }

        fn config(&self) -> &Config {
            self.config
        }

        fn event_loop(&self) -> &EventLoopWindowTarget<Event> {
            unimplemented!();
        }

        fn scheduler_mut(&mut self) -> &mut Scheduler {
            unimplemented!();
        }
    }

    macro_rules! test_clickstate {
        {
            name: $name:ident,
            initial_state: $initial_state:expr,
            initial_button: $initial_button:expr,
            input: $input:expr,
            end_state: $end_state:expr,
        } => {
            #[test]
            fn $name() {
                let cfg = Config::default();
                let size = SizeInfo::new(
                    21.0,
                    51.0,
                    3.0,
                    3.0,
                    0.,
                    0.,
                    false,
                );

                let mut terminal = Term::new(&cfg, size, MockEventProxy);

                let mut mouse = Mouse {
                    click_state: $initial_state,
                    last_click_button: $initial_button,
                    ..Mouse::default()
                };

                let mut message_buffer = MessageBuffer::new();

                let context = ActionContext {
                    terminal: &mut terminal,
                    mouse: &mut mouse,
                    size_info: &size,
                    received_count: 0,
                    suppress_chars: false,
                    modifiers: Default::default(),
                    message_buffer: &mut message_buffer,
                    config: &cfg,
                };

                let mut processor = Processor::new(context);

                let event: GlutinEvent::<'_, TerminalEvent> = $input;
                if let GlutinEvent::WindowEvent {
                    event: WindowEvent::MouseInput {
                        state,
                        button,
                        ..
                    },
                    ..
                } = event
                {
                    processor.mouse_input(state, button);
                };

                assert_eq!(processor.ctx.mouse.click_state, $end_state);
            }
        }
    }

    macro_rules! test_process_binding {
        {
            name: $name:ident,
            binding: $binding:expr,
            triggers: $triggers:expr,
            mode: $mode:expr,
            mods: $mods:expr,
        } => {
            #[test]
            fn $name() {
                if $triggers {
                    assert!($binding.is_triggered_by($mode, $mods, &KEY));
                } else {
                    assert!(!$binding.is_triggered_by($mode, $mods, &KEY));
                }
            }
        }
    }

    test_clickstate! {
        name: single_click,
        initial_state: ClickState::None,
        initial_button: MouseButton::Other(0),
        input: GlutinEvent::WindowEvent {
            event: WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: MouseButton::Left,
                device_id: unsafe { std::mem::transmute_copy(&0) },
                modifiers: ModifiersState::default(),
            },
            window_id: unsafe { std::mem::transmute_copy(&0) },
        },
        end_state: ClickState::Click,
    }

    test_clickstate! {
        name: single_right_click,
        initial_state: ClickState::None,
        initial_button: MouseButton::Other(0),
        input: GlutinEvent::WindowEvent {
            event: WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: MouseButton::Right,
                device_id: unsafe { std::mem::transmute_copy(&0) },
                modifiers: ModifiersState::default(),
            },
            window_id: unsafe { std::mem::transmute_copy(&0) },
        },
        end_state: ClickState::Click,
    }

    test_clickstate! {
        name: single_middle_click,
        initial_state: ClickState::None,
        initial_button: MouseButton::Other(0),
        input: GlutinEvent::WindowEvent {
            event: WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: MouseButton::Middle,
                device_id: unsafe { std::mem::transmute_copy(&0) },
                modifiers: ModifiersState::default(),
            },
            window_id: unsafe { std::mem::transmute_copy(&0) },
        },
        end_state: ClickState::Click,
    }

    test_clickstate! {
        name: multi_click_separate_buttons,
        initial_state: ClickState::Click,
        initial_button: MouseButton::Left,
        input: GlutinEvent::WindowEvent {
            event: WindowEvent::MouseInput {
                state: ElementState::Pressed,
                button: MouseButton::Right,
                device_id: unsafe { std::mem::transmute_copy(&0) },
                modifiers: ModifiersState::default(),
            },
            window_id: unsafe { std::mem::transmute_copy(&0) },
        },
        end_state: ClickState::Click,
    }

    test_process_binding! {
        name: process_binding_nomode_shiftmod_require_shift,
        binding: Binding { trigger: KEY, mods: ModifiersState::SHIFT, action: Action::from("\x1b[1;2D"), mode: BindingMode::empty(), notmode: BindingMode::empty() },
        triggers: true,
        mode: BindingMode::empty(),
        mods: ModifiersState::SHIFT,
    }

    test_process_binding! {
        name: process_binding_nomode_nomod_require_shift,
        binding: Binding { trigger: KEY, mods: ModifiersState::SHIFT, action: Action::from("\x1b[1;2D"), mode: BindingMode::empty(), notmode: BindingMode::empty() },
        triggers: false,
        mode: BindingMode::empty(),
        mods: ModifiersState::empty(),
    }

    test_process_binding! {
        name: process_binding_nomode_controlmod,
        binding: Binding { trigger: KEY, mods: ModifiersState::CTRL, action: Action::from("\x1b[1;5D"), mode: BindingMode::empty(), notmode: BindingMode::empty() },
        triggers: true,
        mode: BindingMode::empty(),
        mods: ModifiersState::CTRL,
    }

    test_process_binding! {
        name: process_binding_nomode_nomod_require_not_appcursor,
        binding: Binding { trigger: KEY, mods: ModifiersState::empty(), action: Action::from("\x1b[D"), mode: BindingMode::empty(), notmode: BindingMode::APP_CURSOR },
        triggers: true,
        mode: BindingMode::empty(),
        mods: ModifiersState::empty(),
    }

    test_process_binding! {
        name: process_binding_appcursormode_nomod_require_appcursor,
        binding: Binding { trigger: KEY, mods: ModifiersState::empty(), action: Action::from("\x1bOD"), mode: BindingMode::APP_CURSOR, notmode: BindingMode::empty() },
        triggers: true,
        mode: BindingMode::APP_CURSOR,
        mods: ModifiersState::empty(),
    }

    test_process_binding! {
        name: process_binding_nomode_nomod_require_appcursor,
        binding: Binding { trigger: KEY, mods: ModifiersState::empty(), action: Action::from("\x1bOD"), mode: BindingMode::APP_CURSOR, notmode: BindingMode::empty() },
        triggers: false,
        mode: BindingMode::empty(),
        mods: ModifiersState::empty(),
    }

    test_process_binding! {
        name: process_binding_appcursormode_appkeypadmode_nomod_require_appcursor,
        binding: Binding { trigger: KEY, mods: ModifiersState::empty(), action: Action::from("\x1bOD"), mode: BindingMode::APP_CURSOR, notmode: BindingMode::empty() },
        triggers: true,
        mode: BindingMode::APP_CURSOR | BindingMode::APP_KEYPAD,
        mods: ModifiersState::empty(),
    }

    test_process_binding! {
        name: process_binding_fail_with_extra_mods,
        binding: Binding { trigger: KEY, mods: ModifiersState::LOGO, action: Action::from("arst"), mode: BindingMode::empty(), notmode: BindingMode::empty() },
        triggers: false,
        mode: BindingMode::empty(),
        mods: ModifiersState::ALT | ModifiersState::LOGO,
    }
}
