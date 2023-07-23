use std::convert::TryInto;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::io;
use anyhow::*;
use superconsole::components::Split;
use superconsole::components::bordering::{Bordered, BorderedSpec};
use superconsole::components::splitting::SplitKind;
use superconsole::{Component, Dimensions, DrawMode, Lines, SuperConsole, Direction, Span};

use crate::sync::lock_failed;

#[derive(Clone)]
pub struct Ui(Arc<Mutex<State>>);

struct State {
	console: SuperConsole,
	jobs: Vec<String>,
	stats: Stats,
}

impl State {
	pub fn render(&mut self) -> Result<()> {
		let bordering = BorderedSpec {
			right: None,
			left: None,
			bottom: None,
			top: Some(Span::dash()),
		};
		let split_bits = vec![
			Stat { desc: "loaded", value: self.stats.load_count },
			Stat { desc: "spawned", value: self.stats.spawn_count },
		];
		let split = Split::new(split_bits, Direction::Horizontal, SplitKind::Equal);
		let root = Bordered::new(&split, bordering);
		self.console.render(&root)?;
		Ok(())
	}
}

#[derive(Debug, Default)]
struct Stats {
	load_count: i32,
	spawn_count: i32,
}

impl Ui {
	pub fn new() -> Self {
		Self(Arc::new(Mutex::new(State {
			// TODO: handle non-TTY
			console: SuperConsole::new().unwrap_or_else(|| panic!()),
			jobs: Default::default(),
			stats: Default::default(),
		})))
	}

	pub fn render(&self) -> Result<()> {
		let mut inner = self.0.lock().map_err(|_| lock_failed("render"))?;
		inner.render()
	}
	
	pub fn writer(&self) -> Writer {
		Writer { inner: self.clone() }
	}
}

struct Stat {
	desc: &'static str,
	value: i32,
}

impl Component for Stat {
	fn draw_unchecked(&self, _dimensions: Dimensions, _mode: DrawMode) -> anyhow::Result<Lines> {
		Ok(Lines(vec![
			vec![
				format!("{}: {}", self.desc, self.value),
			].try_into()?
		]))
	}
}

#[derive(Clone)]
pub struct Writer {
	inner: Ui, // TODO MPSC would be more efficient
}

pub enum Event<'a> {
	Multiline(&'a str),
	Line(&'a str),
	RuleLoad,
	FnInvoke,
	FnReturn,
}

impl Writer {
	pub fn emit<'a>(&self, event: Event<'a>) -> Result<()> {
		let mut inner = self.inner.0.lock().map_err(|_| lock_failed("emit"))?;
		match event {
			Event::FnInvoke => {
				inner.stats.spawn_count += 1;
			},
			Event::Multiline(s) => {
				inner.console.emit(Lines::from_multiline_string(s, Default::default()))
			},
			Event::Line(s) => {
				inner.console.emit(Lines(vec![vec![s.to_owned()].try_into()?]))
			},
			Event::RuleLoad => {
				inner.stats.load_count += 1;
			},
			Event::FnReturn => {
				todo!();
			},
		}
		inner.render()
	}
}

impl std::io::Write for Writer {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		let s = unsafe { std::str::from_utf8_unchecked(buf) };
		self.emit(Event::Multiline(s)).map_err(|e| io::Error::new(io::ErrorKind::Other, e))?;
		io::Result::Ok(buf.len())
	}

	fn flush(&mut self) -> io::Result<()> {
		io::Result::Ok(())
	}
}
