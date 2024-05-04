use clap::*;

#[derive(Parser, Debug)]
#[command(version)]
pub struct CliOpts {
	#[arg(long, short, help="Force rebuilding of named targets (not their dependencies)")]
	pub force: bool,

	#[arg(long, short, help="Set log level to DEBUG")]
	pub verbose: bool,

	#[arg(long, short, help="List targets (additional CLI args are used to filter for a certain prefix)")]
	pub list: bool,
	
	#[arg(allow_hyphen_values=true)]
	pub targets: Vec<String>,
}
