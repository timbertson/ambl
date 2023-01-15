use anyhow::*;
use ambl_api::*;

#[export]
fn get_rules() -> Result<Vec<Rule>> {
	debug("HI");
	Ok(vec!())
}
