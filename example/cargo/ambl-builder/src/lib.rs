use anyhow::*;
use ambl_api::*;

#[export]
fn build(c: TargetCtx) -> Result<()> {
	c.write_dest("Hello world!")
}
