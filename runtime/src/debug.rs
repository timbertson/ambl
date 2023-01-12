use anyhow::*;
use log::*;

use std::ffi::OsString;
use std::path::Path;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};

const DONE: AtomicBool = AtomicBool::new(false);

pub fn shell_on_failure<T>(result: Result<T>) -> Result<T> {
	if let Err(ref e) = result {
		// TODO opt-in via config flag
		if std::env::var_os("AMBL_INTERACTIVE_DEBUG") == Some(OsString::from("1")) {
			if DONE.swap(true, Ordering::SeqCst) == false {
				error!("{:?}", &e);
				warn!("Starting a debug shell; press ctrl^d to continue");
				let mut shell = Command::new("bash");
				shell.arg("-i");
				debug!("Spawning: {:?}", &shell);
				let result = shell.status();
				info!("Interactive shell result: {:?}", result);
			}
		}
	}
	result
}
