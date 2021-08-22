//! Minterm - The GPU Enhanced Terminal.

#![warn(rust_2018_idioms, future_incompatible)]
#![deny(clippy::all, clippy::if_not_else, clippy::enum_glob_use)]
#![cfg_attr(feature = "cargo-clippy", deny(warnings))]

use std::env;
use std::error::Error;
use std::fs;
use std::io::{self, Write};
use std::sync::Arc;

use glutin::event_loop::EventLoop as GlutinEventLoop;
use log::{error, info};

use minterm_terminal::event_loop::{self, EventLoop, Msg};
use minterm_terminal::grid::Dimensions;
use minterm_terminal::sync::FairMutex;
use minterm_terminal::term::Term;
use minterm_terminal::tty;

mod cli;
mod config;
mod daemon;
mod display;
mod event;
mod input;
mod logging;
mod macos;
mod message_bar;
mod renderer;
mod scheduler;

mod gl {
    #![allow(clippy::all)]
    include!(concat!(env!("OUT_DIR"), "/gl_bindings.rs"));
}

use crate::cli::Options;
use crate::config::monitor;
use crate::config::Config;
use crate::display::Display;
use crate::event::{Event, EventProxy, Processor};
use crate::macos::locale;
use crate::message_bar::MessageBuffer;

fn main() {
    // Load command line options.
    let options = Options::new();

    // Setup glutin event loop.
    let window_event_loop = GlutinEventLoop::<Event>::with_user_event();

    // Initialize the logger as soon as possible as to capture output from other subsystems.
    let log_file = logging::initialize(&options, window_event_loop.create_proxy())
        .expect("Unable to initialize logger");

    // Load configuration file.
    let config = config::load(&options);

    // Update the log level from config.
    log::set_max_level(config.ui_config.debug.log_level);

    // Switch to home directory.
    env::set_current_dir(dirs::home_dir().unwrap()).unwrap();
    // Set locale.
    locale::set_locale_environment();

    // Store if log file should be deleted before moving config.
    let persistent_logging = config.ui_config.debug.persistent_logging;

    // Run Minterm.
    if let Err(err) = run(window_event_loop, config, options) {
        error!("Minterm encountered an unrecoverable error:\n\n\t{}\n", err);
        std::process::exit(1);
    }

    // Clean up logfile.
    if let Some(log_file) = log_file {
        if !persistent_logging && fs::remove_file(&log_file).is_ok() {
            let _ = writeln!(io::stdout(), "Deleted log file at \"{}\"", log_file.display());
        }
    }
}

/// Run Minterm.
///
/// Creates a window, the terminal state, PTY, I/O event loop, input processor,
/// config change monitor, and runs the main display loop.
fn run(
    window_event_loop: GlutinEventLoop<Event>,
    config: Config,
    options: Options,
) -> Result<(), Box<dyn Error>> {
    info!("Welcome to Minterm");

    // Log the configuration paths.
    log_config_path(&config);

    // Set environment variables.
    tty::setup_env(&config);

    let event_proxy = EventProxy::new(window_event_loop.create_proxy());

    // Create a display.
    //
    // The display manages a window and can draw the terminal.
    let display = Display::new(&config, &window_event_loop)?;

    info!(
        "PTY dimensions: {:?} x {:?}",
        display.size_info.screen_lines(),
        display.size_info.columns()
    );

    // Create the terminal.
    //
    // This object contains all of the state about what's being displayed. It's
    // wrapped in a clonable mutex since both the I/O loop and display need to
    // access it.
    let terminal = Term::new(&config, display.size_info, event_proxy.clone());
    let terminal = Arc::new(FairMutex::new(terminal));

    // Create the PTY.
    //
    // The PTY forks a process to run the shell on the slave side of the
    // pseudoterminal. A file descriptor for the master side is retained for
    // reading/writing to the shell.
    let pty = tty::new(&config, &display.size_info);

    // Create the pseudoterminal I/O loop.
    //
    // PTY I/O is ran on another thread as to not occupy cycles used by the
    // renderer and input processing. Note that access to the terminal state is
    // synchronized since the I/O loop updates the state, and the display
    // consumes it periodically.
    let event_loop = EventLoop::new(
        Arc::clone(&terminal),
        event_proxy.clone(),
        pty,
        config.hold,
        config.ui_config.debug.ref_test,
    );

    // The event loop channel allows write requests from the event processor
    // to be sent to the pty loop and ultimately written to the pty.
    let loop_tx = event_loop.channel();

    // Create a config monitor when config was loaded from path.
    //
    // The monitor watches the config file for changes and reloads it. Pending
    // config changes are processed in the main loop.
    if config.ui_config.live_config_reload {
        monitor::watch(config.ui_config.config_paths.clone(), event_proxy);
    }

    // Setup storage for message UI.
    let message_buffer = MessageBuffer::new();

    // Event processor.
    let mut processor = Processor::new(
        event_loop::Notifier(loop_tx.clone()),
        message_buffer,
        config,
        display,
        options,
    );

    // Kick off the I/O thread.
    let io_thread = event_loop.spawn();

    info!("Initialisation complete");

    // Start event loop and block until shutdown.
    processor.run(terminal, window_event_loop);

    // TODO: old comments said this was just for windows, so maybe we don't need it
    // drop(processor);

    // Shutdown PTY parser event loop.
    loop_tx.send(Msg::Shutdown).expect("Error sending shutdown to PTY event loop");
    io_thread.join().expect("join io thread");

    // FIXME patch notify library to have a shutdown method.
    // config_reloader.join().ok();

    // Without explicitly detaching the console cmd won't redraw it's prompt.

    info!("Goodbye");

    Ok(())
}

fn log_config_path(config: &Config) {
    if config.ui_config.config_paths.is_empty() {
        return;
    }

    let mut msg = String::from("Configuration files loaded from:");
    for path in &config.ui_config.config_paths {
        msg.push_str(&format!("\n  {:?}", path.display()));
    }

    info!("{}", msg);
}
