#[ambl_api::export]
mod builder {
	use anyhow::*;
	use ambl_api::*;

	fn build(c: TargetCtx) -> Result<()> {
		c.write_dest("Hello world!")
	}
}
