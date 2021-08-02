//! The display subsystem including window management, font rasterization, and
//! GPU drawing.

use std::convert::TryFrom;
use std::f64;
use std::fmt::{self, Formatter};
use std::time::Instant;

use glutin::dpi::{PhysicalPosition, PhysicalSize};
use glutin::event_loop::EventLoop;
use log::{debug, info};
use parking_lot::MutexGuard;

use minterm_crossfont::{self, Rasterize, Rasterizer};

use minterm_terminal::ansi::NamedColor;
use minterm_terminal::event::{EventListener, OnResize};
use minterm_terminal::grid::Dimensions as _;
use minterm_terminal::index::{Column, Line, Point};
use minterm_terminal::term::{SizeInfo, Term, MIN_COLUMNS, MIN_SCREEN_LINES};

use crate::config::font::Font;
use crate::config::window::Dimensions;
use crate::config::Config;
use crate::display::color::List;
use crate::display::content::RenderableContent;
use crate::display::cursor::IntoRects;
use crate::display::meter::Meter;
use crate::display::window::Window;
use crate::message_bar::{MessageBuffer, MessageType};
use crate::renderer::rects::{RenderLines, RenderRect};
use crate::renderer::{self, GlyphCache, QuadRenderer};

pub mod content;
pub mod cursor;
pub mod window;

mod color;
mod meter;

#[derive(Debug)]
pub enum Error {
    /// Error with window management.
    Window(window::Error),

    /// Error dealing with fonts.
    Font(minterm_crossfont::Error),

    /// Error in renderer.
    Render(renderer::Error),

    /// Error during buffer swap.
    Context(glutin::ContextError),
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Window(err) => err.source(),
            Error::Font(err) => err.source(),
            Error::Render(err) => err.source(),
            Error::Context(err) => err.source(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::Window(err) => err.fmt(f),
            Error::Font(err) => err.fmt(f),
            Error::Render(err) => err.fmt(f),
            Error::Context(err) => err.fmt(f),
        }
    }
}

impl From<window::Error> for Error {
    fn from(val: window::Error) -> Self {
        Error::Window(val)
    }
}

impl From<minterm_crossfont::Error> for Error {
    fn from(val: minterm_crossfont::Error) -> Self {
        Error::Font(val)
    }
}

impl From<renderer::Error> for Error {
    fn from(val: renderer::Error) -> Self {
        Error::Render(val)
    }
}

impl From<glutin::ContextError> for Error {
    fn from(val: glutin::ContextError) -> Self {
        Error::Context(val)
    }
}

#[derive(Default, Clone, Debug, PartialEq)]
pub struct DisplayUpdate {
    pub dirty: bool,

    dimensions: Option<PhysicalSize<u32>>,
    cursor_dirty: bool,
    font: Option<Font>,
}

impl DisplayUpdate {
    pub fn dimensions(&self) -> Option<PhysicalSize<u32>> {
        self.dimensions
    }

    pub fn font(&self) -> Option<&Font> {
        self.font.as_ref()
    }

    pub fn cursor_dirty(&self) -> bool {
        self.cursor_dirty
    }

    pub fn set_dimensions(&mut self, dimensions: PhysicalSize<u32>) {
        self.dimensions = Some(dimensions);
        self.dirty = true;
    }

    pub fn set_font(&mut self, font: Font) {
        self.font = Some(font);
        self.dirty = true;
    }

    pub fn set_cursor_dirty(&mut self) {
        self.cursor_dirty = true;
        self.dirty = true;
    }
}

/// The display wraps a window, font rasterizer, and GPU renderer.
pub struct Display {
    pub size_info: SizeInfo,
    pub window: Window,

    /// UI cursor visibility for blinking.
    pub cursor_hidden: bool,

    /// Mapped RGB values for each terminal color.
    pub colors: List,

    renderer: QuadRenderer,
    glyph_cache: GlyphCache,
    meter: Meter,
}

impl Display {
    pub fn new<E>(config: &Config, event_loop: &EventLoop<E>) -> Result<Display, Error> {
        // Guess DPR based on first monitor.
        let estimated_dpr = event_loop.available_monitors().next().map(|m| m.scale_factor()).unwrap_or(1.);

        // Guess the target window dimensions.
        let metrics = GlyphCache::static_metrics(config.ui_config.font.clone(), estimated_dpr)?;
        let (cell_width, cell_height) = compute_cell_size(config, &metrics);

        // Guess the target window size if the user has specified the number of lines/columns.
        let dimensions = config.ui_config.window.dimensions();
        let estimated_size = dimensions.map(|dimensions| {
            window_size(config, dimensions, cell_width, cell_height, estimated_dpr)
        });

        debug!("Estimated DPR: {}", estimated_dpr);
        debug!("Estimated window size: {:?}", estimated_size);
        debug!("Estimated cell size: {} x {}", cell_width, cell_height);

        // Spawn the Minterm window.
        let mut window = Window::new(
            event_loop,
            config,
            estimated_size,
        )?;

        info!("Device pixel ratio: {}", window.dpr);

        // Create renderer.
        let mut renderer = QuadRenderer::new()?;

        let (glyph_cache, cell_width, cell_height) =
            Self::new_glyph_cache(window.dpr, &mut renderer, config)?;

        if let Some(dimensions) = dimensions {
            if (estimated_dpr - window.dpr).abs() < f64::EPSILON {
                info!("Estimated DPR correctly, skipping resize");
            } else {
                // Resize the window again if the DPR was not estimated correctly.
                let size = window_size(config, dimensions, cell_width, cell_height, window.dpr);
                window.set_inner_size(size);
            }
        }

        let padding = config.ui_config.window.padding(window.dpr);
        let viewport_size = window.inner_size();

        // Create new size with at least one column and row.
        let size_info = SizeInfo::new(
            viewport_size.width as f32,
            viewport_size.height as f32,
            cell_width,
            cell_height,
            padding.0,
            padding.1,
            config.ui_config.window.dynamic_padding && dimensions.is_none(),
        );

        info!("Cell size: {} x {}", cell_width, cell_height);
        info!("Padding: {} x {}", size_info.padding_x(), size_info.padding_y());
        info!("Width: {}, Height: {}", size_info.width(), size_info.height());

        // Update OpenGL projection.
        renderer.resize(&size_info);

        // Clear screen.
        let background_color = config.ui_config.colors.primary.background;
        renderer.with_api(&config.ui_config, &size_info, |api| {
            api.clear(background_color);
        });

        // Set subpixel anti-aliasing.
        minterm_crossfont::set_font_smoothing(config.ui_config.font.use_thin_strokes);

        // Disable shadows
        window.set_has_shadow(false);

        window.set_visible(true);

        // Set window position.
        //
        // TODO: replace `set_position` with `with_position` once available.
        // Upstream issue: https://github.com/rust-windowing/winit/issues/806.
        if let Some(position) = config.ui_config.window.position {
            window.set_outer_position(PhysicalPosition::from((position.x, position.y)));
        }

        Ok(Self {
            window,
            renderer,
            glyph_cache,
            meter: Meter::new(),
            size_info,
            cursor_hidden: false,
            colors: List::from(&config.ui_config.colors),
        })
    }

    fn new_glyph_cache(
        dpr: f64,
        renderer: &mut QuadRenderer,
        config: &Config,
    ) -> Result<(GlyphCache, f32, f32), Error> {
        let font = config.ui_config.font.clone();
        let rasterizer = Rasterizer::new(dpr as f32, config.ui_config.font.use_thin_strokes)?;

        // Initialize glyph cache.
        let glyph_cache = {
            info!("Initializing glyph cache...");
            let init_start = Instant::now();

            let cache =
                renderer.with_loader(|mut api| GlyphCache::new(rasterizer, &font, &mut api))?;

            let stop = init_start.elapsed();
            let stop_f = stop.as_secs() as f64 + f64::from(stop.subsec_nanos()) / 1_000_000_000f64;
            info!("... finished initializing glyph cache in {}s", stop_f);

            cache
        };

        // Need font metrics to resize the window properly. This suggests to me the
        // font metrics should be computed before creating the window in the first
        // place so that a resize is not needed.
        let (cw, ch) = compute_cell_size(config, &glyph_cache.font_metrics());

        Ok((glyph_cache, cw, ch))
    }

    /// Update font size and cell dimensions.
    ///
    /// This will return a tuple of the cell width and height.
    fn update_glyph_cache(&mut self, config: &Config, font: &Font) -> (f32, f32) {
        let cache = &mut self.glyph_cache;
        let dpr = self.window.dpr;

        self.renderer.with_loader(|mut api| {
            let _ = cache.update_font_size(font, dpr, &mut api);
        });

        // Compute new cell sizes.
        compute_cell_size(config, &self.glyph_cache.font_metrics())
    }

    /// Clear glyph cache.
    fn clear_glyph_cache(&mut self) {
        let cache = &mut self.glyph_cache;
        self.renderer.with_loader(|mut api| {
            cache.clear_glyph_cache(&mut api);
        });
    }

    /// Process update events.
    pub fn handle_update<T>(
        &mut self,
        terminal: &mut Term<T>,
        pty_resize_handle: &mut dyn OnResize,
        config: &Config,
        update_pending: DisplayUpdate,
    ) where
        T: EventListener,
    {
        let (mut cell_width, mut cell_height) =
            (self.size_info.cell_width(), self.size_info.cell_height());

        // Update font size and cell dimensions.
        if let Some(font) = update_pending.font() {
            let cell_dimensions = self.update_glyph_cache(config, font);
            cell_width = cell_dimensions.0;
            cell_height = cell_dimensions.1;

            info!("Cell size: {} x {}", cell_width, cell_height);
        } else if update_pending.cursor_dirty() {
            self.clear_glyph_cache();
        }

        let (mut width, mut height) = (self.size_info.width(), self.size_info.height());
        if let Some(dimensions) = update_pending.dimensions() {
            width = dimensions.width as f32;
            height = dimensions.height as f32;
        }

        let padding = config.ui_config.window.padding(self.window.dpr);

        self.size_info = SizeInfo::new(
            width,
            height,
            cell_width,
            cell_height,
            padding.0,
            padding.1,
            config.ui_config.window.dynamic_padding,
        );

        // Resize PTY.
        pty_resize_handle.on_resize(&self.size_info);

        // Resize terminal.
        terminal.resize(self.size_info);

        // Resize renderer.
        let physical =
            PhysicalSize::new(self.size_info.width() as u32, self.size_info.height() as u32);
        self.window.resize(physical);
        self.renderer.resize(&self.size_info);

        info!("Padding: {} x {}", self.size_info.padding_x(), self.size_info.padding_y());
        info!("Width: {}, Height: {}", self.size_info.width(), self.size_info.height());
    }

    /// Draw the screen.
    ///
    /// A reference to Term whose state is being drawn must be provided.
    ///
    /// This call may block if vsync is enabled.
    pub fn draw<T: EventListener>(
        &mut self,
        terminal: MutexGuard<'_, Term<T>>,
        message_buffer: &MessageBuffer,
        config: &Config,
    ) {
        // Collect renderable content before the terminal is dropped.
        let mut content = RenderableContent::new(config, self, &terminal);
        let mut grid_cells = Vec::new();
        for cell in &mut content {
            grid_cells.push(cell);
        }
        let background_color = content.color(NamedColor::Background as usize);
        let cursor = content.cursor();

        let cursor_point = terminal.grid().cursor.point;
        let metrics = self.glyph_cache.font_metrics();
        let size_info = self.size_info;

        // Drop terminal as early as possible to free lock.
        drop(terminal);

        self.renderer.with_api(&config.ui_config, &size_info, |api| {
            api.clear(background_color);
        });

        let mut lines = RenderLines::new();

        // Draw grid.
        {
            let _sampler = self.meter.sampler();

            let glyph_cache = &mut self.glyph_cache;
            self.renderer.with_api(&config.ui_config, &size_info, |mut api| {
                // Iterate over all non-empty cells in the grid.
                for cell in grid_cells {
                    // Update underline/strikeout.
                    lines.update(&cell);

                    // Draw the cell.
                    api.render_cell(cell, glyph_cache);
                }
            });
        }

        let mut rects = lines.rects(&metrics, &size_info);

        // Push the cursor rects for rendering.
        if let Some(cursor) = cursor {
            for rect in cursor.rects(&size_info, config.cursor.thickness()) {
                rects.push(rect);
            }
        }

        if let Some(message) = message_buffer.message() {
            let text = message.text(&size_info);

            // Create a new rectangle for the background.
            let start_line = size_info.screen_lines();
            let y = size_info.cell_height().mul_add(start_line as f32, size_info.padding_y());

            let bg = match message.ty() {
                MessageType::Error => config.ui_config.colors.normal.red,
                MessageType::Warning => config.ui_config.colors.normal.yellow,
            };

            let message_bar_rect =
                RenderRect::new(0., y, size_info.width(), size_info.height() - y, bg, 1.);

            // Push message_bar in the end, so it'll be above all other content.
            rects.push(message_bar_rect);

            // Draw rectangles.
            self.renderer.draw_rects(&size_info, rects);

            // Relay messages to the user.
            let glyph_cache = &mut self.glyph_cache;
            let fg = config.ui_config.colors.primary.background;
            for (i, message_text) in text.iter().enumerate() {
                let point = Point::new(start_line + i, Column(0));
                self.renderer.with_api(&config.ui_config, &size_info, |mut api| {
                    api.render_string(glyph_cache, point, fg, bg, message_text);
                });
            }
        } else {
            // Draw rectangles.
            self.renderer.draw_rects(&size_info, rects);
        }

        self.draw_render_timer(config, &size_info);
        // Handle IME positioning.
        let ime_position = cursor_point;

        // Update IME position.
        self.window.update_ime_position(ime_position, &self.size_info);

        self.window.swap_buffers();
    }

    /// Update to a new configuration.
    pub fn update_config(&mut self, config: &Config) {
        self.colors = List::from(&config.ui_config.colors);
    }

    /// Draw render timer.
    fn draw_render_timer(&mut self, config: &Config, size_info: &SizeInfo) {
        if !config.ui_config.debug.render_timer {
            return;
        }

        let glyph_cache = &mut self.glyph_cache;

        let timing = format!("{:.3} usec", self.meter.average());
        let point = Point::new(size_info.screen_lines().saturating_sub(2), Column(0));
        let fg = config.ui_config.colors.primary.background;
        let bg = config.ui_config.colors.normal.red;

        self.renderer.with_api(&config.ui_config, size_info, |mut api| {
            api.render_string(glyph_cache, point, fg, bg, &timing);
        });
    }
}

/// Convert a terminal point to a viewport relative point.
pub fn point_to_viewport(display_offset: usize, point: Point) -> Option<Point<usize>> {
    let viewport_line = point.line.0 + display_offset as i32;
    usize::try_from(viewport_line).ok().map(|line| Point::new(line, point.column))
}

/// Convert a viewport relative point to a terminal point.
pub fn viewport_to_point(display_offset: usize, point: Point<usize>) -> Point {
    let line = Line(point.line as i32) - display_offset;
    Point::new(line, point.column)
}

/// Calculate the cell dimensions based on font metrics.
///
/// This will return a tuple of the cell width and height.
#[inline]
fn compute_cell_size(config: &Config, metrics: &minterm_crossfont::Metrics) -> (f32, f32) {
    let offset_x = f64::from(config.ui_config.font.offset.x);
    let offset_y = f64::from(config.ui_config.font.offset.y);
    (
        (metrics.average_advance + offset_x).floor().max(1.) as f32,
        (metrics.line_height + offset_y).floor().max(1.) as f32,
    )
}

/// Calculate the size of the window given padding, terminal dimensions and cell size.
fn window_size(
    config: &Config,
    dimensions: Dimensions,
    cell_width: f32,
    cell_height: f32,
    dpr: f64,
) -> PhysicalSize<u32> {
    let padding = config.ui_config.window.padding(dpr);

    let grid_width = cell_width * dimensions.columns.0.max(MIN_COLUMNS) as f32;
    let grid_height = cell_height * dimensions.lines.max(MIN_SCREEN_LINES) as f32;

    let width = (padding.0).mul_add(2., grid_width).floor();
    let height = (padding.1).mul_add(2., grid_height).floor();

    PhysicalSize::new(width as u32, height as u32)
}
