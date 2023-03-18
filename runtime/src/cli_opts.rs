use clap::*;

#[derive(Parser, Debug)]
#[command(version)]
pub struct CliOpts {

	#[arg(long, short)]
	pub force: bool,

	#[arg(long, short)]
	pub verbose: bool,

	#[arg(long, short)]
	pub list: bool,
}
