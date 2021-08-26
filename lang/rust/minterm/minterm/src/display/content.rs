use std::mem;

use minterm_terminal::ansi::{Color, CursorShape, NamedColor};
use minterm_terminal::config::Config;
use minterm_terminal::event::EventListener;
use minterm_terminal::grid::{Indexed};
use minterm_terminal::index::{Point};
use minterm_terminal::term::cell::{Cell, Flags};
use minterm_terminal::term::color::{CellRgb, Rgb};
use minterm_terminal::term::{RenderableContent as TerminalContent, Term};

use crate::config::ui_config::UiConfig;
use crate::display::color::{List, DIM_FACTOR};
use crate::display::{self, Display};

/// Minimum contrast between a fixed cursor color and the cell's background.
pub const MIN_CURSOR_CONTRAST: f64 = 1.5;

/// Renderable terminal content.
///
/// This provides the terminal cursor and an iterator over all non-empty cells.
pub struct RenderableContent<'a> {
    terminal_content: TerminalContent<'a>,
    cursor: Option<RenderableCursor>,
    cursor_shape: CursorShape,
    cursor_point: Point<usize>,
    config: &'a Config<UiConfig>,
    colors: &'a List,
}

impl<'a> RenderableContent<'a> {
    pub fn new<T: EventListener>(
        config: &'a Config<UiConfig>,
        display: &'a mut Display,
        term: &'a Term<T>,
    ) -> Self {
        let terminal_content = term.renderable_content();

        // Find terminal cursor shape.
        let cursor_shape = if terminal_content.cursor.shape == CursorShape::Hidden
            || display.cursor_hidden
        {
            CursorShape::Hidden
        } else if !term.is_focused && config.cursor.unfocused_hollow {
            CursorShape::HollowBlock
        } else {
            terminal_content.cursor.shape
        };

        // Convert terminal cursor point to viewport position.
        let cursor_point = terminal_content.cursor.point;
        let display_offset = terminal_content.display_offset;
        let cursor_point = display::point_to_viewport(display_offset, cursor_point).unwrap();

        Self {
            colors: &display.colors,
            cursor: None,
            terminal_content,
            cursor_shape,
            cursor_point,
            config,
        }
    }

    /// Get the terminal cursor.
    pub fn cursor(mut self) -> Option<RenderableCursor> {
        // Assure this function is only called after the iterator has been drained.
        debug_assert!(self.next().is_none());

        self.cursor
    }

    /// Get the RGB value for a color index.
    pub fn color(&self, color: usize) -> Rgb {
        self.terminal_content.colors[color].unwrap_or(self.colors[color])
    }

    /// Assemble the information required to render the terminal cursor.
    ///
    /// This will return `None` when there is no cursor visible.
    fn renderable_cursor(&mut self, cell: &RenderableCell) -> Option<RenderableCursor> {
        if self.cursor_shape == CursorShape::Hidden {
            return None;
        }

        // Cursor colors.
        let color = self.config.ui_config.colors.cursor;
        let mut cursor_color =
            self.terminal_content.colors[NamedColor::Cursor].map_or(color.background, CellRgb::Rgb);
        let mut text_color = color.foreground;

        // Invert the cursor if it has a fixed background close to the cell's background.
        if matches!(
            cursor_color,
            CellRgb::Rgb(color) if color.contrast(cell.bg) < MIN_CURSOR_CONTRAST
        ) {
            cursor_color = CellRgb::CellForeground;
            text_color = CellRgb::CellBackground;
        }

        // Convert from cell colors to RGB.
        let text_color = text_color.color(cell.fg, cell.bg);
        let cursor_color = cursor_color.color(cell.fg, cell.bg);

        Some(RenderableCursor {
            is_wide: cell.flags.contains(Flags::WIDE_CHAR),
            shape: self.cursor_shape,
            point: self.cursor_point,
            cursor_color,
            text_color,
        })
    }
}

impl<'a> Iterator for RenderableContent<'a> {
    type Item = RenderableCell;

    /// Gets the next renderable cell.
    ///
    /// Skips empty (background) cells and applies any flags to the cell state
    /// (eg. invert fg and bg colors).
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let cell = self.terminal_content.display_iter.next()?;
            let mut cell = RenderableCell::new(self, cell);

            if self.cursor_point == cell.point {
                // Store the cursor which should be rendered.
                self.cursor = self.renderable_cursor(&cell).map(|cursor| {
                    if cursor.shape == CursorShape::Block {
                        cell.fg = cursor.text_color;
                        cell.bg = cursor.cursor_color;

                        // Since we draw Block cursor by drawing cell below it with a proper color,
                        // we must adjust alpha to make it visible.
                        cell.bg_alpha = 1.;
                    }

                    cursor
                });

                return Some(cell);
            } else if !cell.is_empty() && !cell.flags.contains(Flags::WIDE_CHAR_SPACER) {
                // Skip empty cells and wide char spacers.
                return Some(cell);
            }
        }
    }
}

/// Cell ready for rendering.
#[derive(Clone, Debug)]
pub struct RenderableCell {
    pub character: char,
    pub zerowidth: Option<Vec<char>>,
    pub point: Point<usize>,
    pub fg: Rgb,
    pub bg: Rgb,
    pub bg_alpha: f32,
    pub flags: Flags,
}

impl RenderableCell {
    fn new<'a>(content: &mut RenderableContent<'a>, cell: Indexed<&Cell>) -> Self {
        // Lookup RGB values.
        let mut fg = Self::compute_fg_rgb(content, cell.fg, cell.flags);
        let mut bg = Self::compute_bg_rgb(content, cell.bg);

        let bg_alpha = if cell.flags.contains(Flags::INVERSE) {
            mem::swap(&mut fg, &mut bg);
            1.0
        } else {
            Self::compute_bg_alpha(cell.bg)
        };

        let display_offset = content.terminal_content.display_offset;
        let character = cell.c;

        // Convert cell point to viewport position.
        let cell_point = cell.point;
        let point = display::point_to_viewport(display_offset, cell_point).unwrap();

        RenderableCell {
            zerowidth: cell.zerowidth().map(|zerowidth| zerowidth.to_vec()),
            flags: cell.flags,
            character,
            bg_alpha,
            point,
            fg,
            bg,
        }
    }

    /// Check if cell contains any renderable content.
    fn is_empty(&self) -> bool {
        self.bg_alpha == 0.
            && self.character == ' '
            && self.zerowidth.is_none()
            && !self.flags.intersects(Flags::UNDERLINE | Flags::STRIKEOUT | Flags::DOUBLE_UNDERLINE)
    }

    /// Get the RGB color from a cell's foreground color.
    fn compute_fg_rgb(content: &mut RenderableContent<'_>, fg: Color, flags: Flags) -> Rgb {
        let ui_config = &content.config.ui_config;
        match fg {
            Color::Spec(rgb) => match flags & Flags::DIM {
                Flags::DIM => rgb * DIM_FACTOR,
                _ => rgb,
            },
            Color::Named(ansi) => {
                match (ui_config.draw_bold_text_with_bright_colors, flags & Flags::DIM_BOLD) {
                    // If no bright foreground is set, treat it like the BOLD flag doesn't exist.
                    (_, Flags::DIM_BOLD)
                        if ansi == NamedColor::Foreground
                            && ui_config.colors.primary.bright_foreground.is_none() =>
                    {
                        content.color(NamedColor::DimForeground as usize)
                    },
                    // Draw bold text in bright colors *and* contains bold flag.
                    (true, Flags::BOLD) => content.color(ansi.to_bright() as usize),
                    // Cell is marked as dim and not bold.
                    (_, Flags::DIM) | (false, Flags::DIM_BOLD) => {
                        content.color(ansi.to_dim() as usize)
                    },
                    // None of the above, keep original color..
                    _ => content.color(ansi as usize),
                }
            },
            Color::Indexed(idx) => {
                let idx = match (
                    ui_config.draw_bold_text_with_bright_colors,
                    flags & Flags::DIM_BOLD,
                    idx,
                ) {
                    (true, Flags::BOLD, 0..=7) => idx as usize + 8,
                    (false, Flags::DIM, 8..=15) => idx as usize - 8,
                    (false, Flags::DIM, 0..=7) => NamedColor::DimBlack as usize + idx as usize,
                    _ => idx as usize,
                };

                content.color(idx)
            },
        }
    }

    /// Get the RGB color from a cell's background color.
    #[inline]
    fn compute_bg_rgb(content: &mut RenderableContent<'_>, bg: Color) -> Rgb {
        match bg {
            Color::Spec(rgb) => rgb,
            Color::Named(ansi) => content.color(ansi as usize),
            Color::Indexed(idx) => content.color(idx as usize),
        }
    }

    /// Compute background alpha based on cell's original color.
    ///
    /// Since an RGB color matching the background should not be transparent, this is computed
    /// using the named input color, rather than checking the RGB of the background after its color
    /// is computed.
    #[inline]
    fn compute_bg_alpha(bg: Color) -> f32 {
        if bg == Color::Named(NamedColor::Background) {
            0.
        } else {
            1.
        }
    }
}

/// Cursor storing all information relevant for rendering.
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct RenderableCursor {
    shape: CursorShape,
    cursor_color: Rgb,
    text_color: Rgb,
    is_wide: bool,
    point: Point<usize>,
}

impl RenderableCursor {
    pub fn color(&self) -> Rgb {
        self.cursor_color
    }

    pub fn shape(&self) -> CursorShape {
        self.shape
    }

    pub fn is_wide(&self) -> bool {
        self.is_wide
    }

    pub fn point(&self) -> Point<usize> {
        self.point
    }
}
