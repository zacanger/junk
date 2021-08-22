use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::{env, fs, io};

use log::{error, info};
use serde::Deserialize;
use serde_yaml::mapping::Mapping;
use serde_yaml::Value;

use minterm_terminal::config::{Config as TermConfig, LOG_TARGET_CONFIG};

pub mod color;
pub mod debug;
pub mod font;
pub mod monitor;
pub mod serde_utils;
pub mod ui_config;
pub mod window;

mod bindings;
mod mouse;

use crate::cli::Options;
pub use crate::config::bindings::{Action, Binding, BindingMode, Key};
#[cfg(test)]
pub use crate::config::mouse::{ClickHandler, Mouse};
use crate::config::ui_config::UiConfig;

pub type Config = TermConfig<UiConfig>;

/// Result from config loading.
pub type Result<T> = std::result::Result<T, Error>;

/// Errors occurring during config loading.
#[derive(Debug)]
pub enum Error {
    /// Config file not found.
    NotFound,

    /// Couldn't read $HOME environment variable.
    ReadingEnvHome(env::VarError),

    /// io error reading file.
    Io(io::Error),

    /// Not valid yaml or missing parameters.
    Yaml(serde_yaml::Error),
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::NotFound => None,
            Error::ReadingEnvHome(err) => err.source(),
            Error::Io(err) => err.source(),
            Error::Yaml(err) => err.source(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::NotFound => write!(f, "Unable to locate config file"),
            Error::ReadingEnvHome(err) => {
                write!(f, "Unable to read $HOME environment variable: {}", err)
            },
            Error::Io(err) => write!(f, "Error reading config file: {}", err),
            Error::Yaml(err) => write!(f, "Config error: {}", err),
        }
    }
}

impl From<env::VarError> for Error {
    fn from(val: env::VarError) -> Self {
        Error::ReadingEnvHome(val)
    }
}

impl From<io::Error> for Error {
    fn from(val: io::Error) -> Self {
        if val.kind() == io::ErrorKind::NotFound {
            Error::NotFound
        } else {
            Error::Io(val)
        }
    }
}

impl From<serde_yaml::Error> for Error {
    fn from(val: serde_yaml::Error) -> Self {
        Error::Yaml(val)
    }
}

/// Load the configuration file.
pub fn load(options: &Options) -> Config {
    let config_options = options.config_options().clone();
    let config_path = options.config_path().or_else(installed_config);

    // Load the config using the following fallback behavior:
    //  - Config path + CLI overrides
    //  - CLI overrides
    //  - Default
    let mut config = config_path
        .as_ref()
        .and_then(|config_path| load_from(config_path, config_options.clone()).ok())
        .unwrap_or_else(|| {
            let mut config = Config::deserialize(config_options).unwrap_or_default();
            match config_path {
                Some(config_path) => config.ui_config.config_paths.push(config_path),
                None => info!(target: LOG_TARGET_CONFIG, "No config file found; using default"),
            }
            config
        });

    after_loading(&mut config, options);

    config
}

/// Attempt to reload the configuration file.
pub fn reload(config_path: &Path, options: &Options) -> Result<Config> {
    // Load config, propagating errors.
    let config_options = options.config_options().clone();
    let mut config = load_from(config_path, config_options)?;

    after_loading(&mut config, options);

    Ok(config)
}

/// Modifications after the `Config` object is created.
fn after_loading(config: &mut Config, options: &Options) {
    // Override config with CLI options.
    options.override_config(config);
}

/// Load configuration file and log errors.
fn load_from(path: &Path, cli_config: Value) -> Result<Config> {
    match read_config(path, cli_config) {
        Ok(config) => Ok(config),
        Err(err) => {
            error!(target: LOG_TARGET_CONFIG, "Unable to load config {:?}: {}", path, err);
            Err(err)
        },
    }
}

/// Deserialize configuration file from path.
fn read_config(path: &Path, cli_config: Value) -> Result<Config> {
    let mut config_paths = Vec::new();
    let mut config_value = parse_config(path, &mut config_paths)?;

    // Override config with CLI options.
    config_value = serde_utils::merge(config_value, cli_config);

    // Deserialize to concrete type.
    let mut config = Config::deserialize(config_value)?;
    config.ui_config.config_paths = config_paths;

    Ok(config)
}

/// Deserialize all configuration files as generic Value.
fn parse_config(
    path: &Path,
    config_paths: &mut Vec<PathBuf>,
) -> Result<Value> {
    config_paths.push(path.to_owned());

    let mut contents = fs::read_to_string(path)?;

    // Remove UTF-8 BOM.
    if contents.starts_with('\u{FEFF}') {
        contents = contents.split_off(3);
    }

    // Load configuration file as Value.
    let config: Value = match serde_yaml::from_str(&contents) {
        Ok(config) => config,
        Err(error) => {
            // Prevent parsing error with an empty string and commented out file.
            if error.to_string() == "EOF while parsing a value" {
                Value::Mapping(Mapping::new())
            } else {
                return Err(Error::Yaml(error));
            }
        },
    };

    Ok(config)
}

/// Resolve the config file
fn installed_config() -> Option<PathBuf> {
    if let Ok(home) = env::var("HOME") {
        let p = PathBuf::from(&home).join(".minterm.yml");
        if p.exists() {
            return Some(p);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    static DEFAULT_MINTERM_CONFIG: &str =
        concat!(env!("CARGO_MANIFEST_DIR"), "/../minterm.yml");

    #[test]
    fn config_read_eof() {
        let config_path: PathBuf = DEFAULT_MINTERM_CONFIG.into();
        let mut config = read_config(&config_path, Value::Null).unwrap();
        config.ui_config.config_paths = Vec::new();
        assert_eq!(config, Config::default());
    }
}
