use std::ffi::OsStr;
use std::fmt::Debug;
use std::io;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

use log::{debug, warn};

/// Start the daemon and log error on failure.
pub fn start_daemon<I, S>(program: &str, args: I)
where
    I: IntoIterator<Item = S> + Debug + Copy,
    S: AsRef<OsStr>,
{
    match spawn_daemon(program, args) {
        Ok(_) => debug!("Launched {} with args {:?}", program, args),
        Err(_) => warn!("Unable to launch {} with args {:?}", program, args),
    }
}

fn spawn_daemon<I, S>(program: &str, args: I) -> io::Result<()>
where
    I: IntoIterator<Item = S> + Copy,
    S: AsRef<OsStr>,
{
    unsafe {
        Command::new(program)
            .args(args)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .pre_exec(|| {
                match libc::fork() {
                    -1 => return Err(io::Error::last_os_error()),
                    0 => (),
                    _ => libc::_exit(0),
                }

                if libc::setsid() == -1 {
                    return Err(io::Error::last_os_error());
                }

                Ok(())
            })
            .spawn()?
            .wait()
            .map(|_| ())
    }
}
