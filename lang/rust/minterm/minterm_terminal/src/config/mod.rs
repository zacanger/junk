use std::collections::HashMap;
use std::path::PathBuf;

use serde::Deserialize;

use minterm_config_derive::ConfigDeserialize;

mod scrolling;

use crate::ansi::{CursorShape, CursorStyle};

pub use crate::config::scrolling::Scrolling;

pub const LOG_TARGET_CONFIG: &str = "minterm_config_derive";

pub type MockConfig = Config<HashMap<String, serde_yaml::Value>>;

/// Top-level config type.
#[derive(ConfigDeserialize, Debug, PartialEq, Default)]
pub struct Config<T> {
    /// TERM env variable.
    pub env: HashMap<String, String>,

    /// Path to a shell program to run on startup.
    pub shell: Option<Program>,

    /// How much scrolling history to keep.
    pub scrolling: Scrolling,

    /// Cursor configuration.
    pub cursor: Cursor,

    /// Shell startup directory.
    pub working_directory: Option<PathBuf>,

    /// Additional configuration options not directly required by the terminal.
    #[config(flatten)]
    pub ui_config: T,

    /// Remain open after child process exits.
    #[config(skip)]
    pub hold: bool,
}

#[derive(ConfigDeserialize, Copy, Clone, Debug, PartialEq)]
pub struct Cursor {
    pub style: ConfigCursorStyle,
    pub unfocused_hollow: bool,

    thickness: Percentage,
}

impl Default for Cursor {
    fn default() -> Self {
        Self {
            thickness: Percentage(0.15),
            unfocused_hollow: true,
            style: Default::default(),
        }
    }
}

impl Cursor {
    #[inline]
    pub fn thickness(self) -> f32 {
        self.thickness.as_f32()
    }

    #[inline]
    pub fn style(self) -> CursorStyle {
        self.style.into()
    }
}

#[derive(Deserialize, Debug, Copy, Clone, PartialEq, Eq)]
#[serde(untagged)]
pub enum ConfigCursorStyle {
    Shape(CursorShape),
}

impl Default for ConfigCursorStyle {
    fn default() -> Self {
        Self::Shape(CursorShape::default())
    }
}

impl ConfigCursorStyle {
}

impl From<ConfigCursorStyle> for CursorStyle {
    fn from(config_style: ConfigCursorStyle) -> Self {
        match config_style {
            ConfigCursorStyle::Shape(shape) => Self { shape },
        }
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(untagged)]
pub enum Program {
    Just(String),
    WithArgs {
        program: String,
        #[serde(default)]
        args: Vec<String>,
    },
}

impl Program {
    pub fn program(&self) -> &str {
        match self {
            Program::Just(program) => program,
            Program::WithArgs { program, .. } => program,
        }
    }

    pub fn args(&self) -> &[String] {
        match self {
            Program::Just(_) => &[],
            Program::WithArgs { args, .. } => args,
        }
    }
}

/// Wrapper around f32 that represents a percentage value between 0.0 and 1.0.
#[derive(Deserialize, Clone, Copy, Debug, PartialEq)]
pub struct Percentage(f32);

impl Default for Percentage {
    fn default() -> Self {
        Percentage(1.0)
    }
}

impl Percentage {
    pub fn new(value: f32) -> Self {
        Percentage(if value < 0.0 {
            0.0
        } else if value > 1.0 {
            1.0
        } else {
            value
        })
    }

    pub fn as_f32(self) -> f32 {
        self.0
    }
}
