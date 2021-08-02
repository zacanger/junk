#[rustfmt::skip]
use std::fmt::{self, Display, Formatter};

use cocoa::base::{id, NO, YES};
use glutin::dpi::{PhysicalPosition, PhysicalSize};
use glutin::event_loop::EventLoop;
use glutin::platform::macos::{WindowBuilderExtMacOS};
use glutin::window::{
    CursorIcon, UserAttentionType, Window as GlutinWindow, WindowBuilder, WindowId,
};
use glutin::{self, ContextBuilder, PossiblyCurrent, WindowedContext};
use objc::{msg_send, sel, sel_impl};
use raw_window_handle::{HasRawWindowHandle, RawWindowHandle};

use minterm_terminal::index::Point;
use minterm_terminal::term::SizeInfo;

use crate::config::Config;
use crate::gl;

/// Window errors.
#[derive(Debug)]
pub enum Error {
    /// Error creating the window.
    ContextCreation(glutin::CreationError),

    /// Error dealing with fonts.
    Font(minterm_crossfont::Error),

    /// Error manipulating the rendering context.
    Context(glutin::ContextError),
}

/// Result of fallible operations concerning a Window.
type Result<T> = std::result::Result<T, Error>;

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::ContextCreation(err) => err.source(),
            Error::Context(err) => err.source(),
            Error::Font(err) => err.source(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::ContextCreation(err) => write!(f, "Error creating GL context; {}", err),
            Error::Context(err) => write!(f, "Error operating on render context; {}", err),
            Error::Font(err) => err.fmt(f),
        }
    }
}

impl From<glutin::CreationError> for Error {
    fn from(val: glutin::CreationError) -> Self {
        Error::ContextCreation(val)
    }
}

impl From<glutin::ContextError> for Error {
    fn from(val: glutin::ContextError) -> Self {
        Error::Context(val)
    }
}

impl From<minterm_crossfont::Error> for Error {
    fn from(val: minterm_crossfont::Error) -> Self {
        Error::Font(val)
    }
}

fn create_gl_window<E>(
    mut window: WindowBuilder,
    event_loop: &EventLoop<E>,
    srgb: bool,
    dimensions: Option<PhysicalSize<u32>>,
) -> Result<WindowedContext<PossiblyCurrent>> {
    if let Some(dimensions) = dimensions {
        window = window.with_inner_size(dimensions);
    }

    let windowed_context = ContextBuilder::new()
        .with_srgb(srgb)
        .with_hardware_acceleration(None)
        .build_windowed(window, event_loop)?;

    // Make the context current so OpenGL operations can run.
    let windowed_context = unsafe { windowed_context.make_current().map_err(|(_, err)| err)? };

    Ok(windowed_context)
}

/// A window which can be used for displaying the terminal.
///
/// Wraps the underlying windowing library to provide a stable API in Minterm.
pub struct Window {
    /// Cached DPR for quickly scaling pixel sizes.
    pub dpr: f64,

    windowed_context: WindowedContext<PossiblyCurrent>,
    current_mouse_cursor: CursorIcon,
    mouse_visible: bool,
}

impl Window {
    /// Create a new window.
    ///
    /// This creates a window and fully initializes a window.
    pub fn new<E>(
        event_loop: &EventLoop<E>,
        config: &Config,
        size: Option<PhysicalSize<u32>>,
    ) -> Result<Window> {
        let window_config = &config.ui_config.window;
        let window_builder = Window::get_platform_window(&window_config.title);

        let windowed_context =
            create_gl_window(window_builder.clone(), event_loop, false, size)
                .or_else(|_| {
                    create_gl_window(window_builder, event_loop, true, size)
                })?;

        // Text cursor.
        let current_mouse_cursor = CursorIcon::Text;
        windowed_context.window().set_cursor_icon(current_mouse_cursor);

        // Set OpenGL symbol loader. This call MUST be after window.make_current on windows.
        gl::load_with(|symbol| windowed_context.get_proc_address(symbol) as *const _);

        #[allow(unused_mut)]
        let mut dpr = windowed_context.window().scale_factor();

        Ok(Self {
            current_mouse_cursor,
            mouse_visible: true,
            windowed_context,
            dpr,
        })
    }

    pub fn set_inner_size(&mut self, size: PhysicalSize<u32>) {
        self.window().set_inner_size(size);
    }

    pub fn inner_size(&self) -> PhysicalSize<u32> {
        self.window().inner_size()
    }

    #[inline]
    pub fn set_visible(&self, visibility: bool) {
        self.window().set_visible(visibility);
    }

    #[inline]
    pub fn set_mouse_cursor(&mut self, cursor: CursorIcon) {
        if cursor != self.current_mouse_cursor {
            self.current_mouse_cursor = cursor;
            self.window().set_cursor_icon(cursor);
        }
    }

    /// Set mouse cursor visible.
    pub fn set_mouse_visible(&mut self, visible: bool) {
        if visible != self.mouse_visible {
            self.mouse_visible = visible;
            self.window().set_cursor_visible(visible);
        }
    }

    pub fn get_platform_window(title: &str) -> WindowBuilder {
        return WindowBuilder::new()
            .with_title(title)
            .with_visible(false)
            .with_transparent(true)
            .with_titlebar_hidden(true);
    }

    pub fn set_urgent(&self, is_urgent: bool) {
        let attention = if is_urgent { Some(UserAttentionType::Critical) } else { None };

        self.window().request_user_attention(attention);
    }

    pub fn set_outer_position(&self, pos: PhysicalPosition<i32>) {
        self.window().set_outer_position(pos);
    }

    pub fn window_id(&self) -> WindowId {
        self.window().id()
    }

    /// Adjust the IME editor position according to the new location of the cursor.
    pub fn update_ime_position(&mut self, point: Point, size: &SizeInfo) {
        let nspot_x = f64::from(size.padding_x() + point.column.0 as f32 * size.cell_width());
        let nspot_y = f64::from(size.padding_y() + (point.line.0 + 1) as f32 * size.cell_height());

        self.window().set_ime_position(PhysicalPosition::new(nspot_x, nspot_y));
    }

    pub fn swap_buffers(&self) {
        self.windowed_context.swap_buffers().expect("swap buffers");
    }

    pub fn resize(&self, size: PhysicalSize<u32>) {
        self.windowed_context.resize(size);
    }

    /// Disable macOS window shadows.
    ///
    /// This prevents rendering artifacts from showing up when the window is transparent.
    pub fn set_has_shadow(&self, has_shadows: bool) {
        let raw_window = match self.window().raw_window_handle() {
            RawWindowHandle::MacOS(handle) => handle.ns_window as id,
            _ => return,
        };

        let value = if has_shadows { YES } else { NO };
        unsafe {
            let _: () = msg_send![raw_window, setHasShadow: value];
        }
    }

    fn window(&self) -> &GlutinWindow {
        self.windowed_context.window()
    }
}
