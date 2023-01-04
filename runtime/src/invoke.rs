use std::fs;

use anyhow::*;
use log::*;
use ambl_common::build::{InvokeResponse, InvokeAction};

pub fn perform(action: InvokeAction) -> Result<InvokeResponse> {
	match action {
		InvokeAction::WriteFile(f) => {
			debug!("Writing file {}", &f.path);
			fs::write(&f.path, &f.contents)?;
		},
		InvokeAction::CopyFile(f) => {
			debug!("Copying file {} -> {}", &f.src, &f.dest);
			fs::copy(&f.src, &f.dest)?;
		},
	}
	Ok(InvokeResponse::Unit)
}
