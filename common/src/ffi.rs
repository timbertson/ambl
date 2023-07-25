use anyhow::*;
use serde::{Serialize, Deserialize, de::DeserializeOwned};

#[cfg(target_arch = "wasm32")]
wit_bindgen::generate!({
	world: "builder",
	export_macro_name: "ambl_export_builder_raw",
	macro_export,
});

#[cfg(not(target_arch = "wasm32"))]
pub fn ambllog(_: u8, _: &str) {}

#[cfg(not(target_arch = "wasm32"))]
pub fn amblinvoke(_: &str) -> String { panic!("stub") }

#[derive(Serialize, Deserialize)]
pub enum ResultFFI<T> {
	Ok(T),
	Err(Vec<String>),
}

impl<T> ResultFFI<T> {
	pub fn into_result(self) -> Result<T> {
		match self {
			ResultFFI::Ok(t) => Result::Ok(t),
			ResultFFI::Err(chain) => {
				let mut it = chain.into_iter();
				let mut err = Error::msg(it.next().unwrap_or_else(|| "[empty error message]".to_owned()));
				for msg in it {
					err = err.context(msg);
				}
				Result::Err(err)
			},
		}
	}
	
	// TODO: return Vec instead of String?
	pub fn serialize(r: Result<T>) -> String where T: Serialize {
		serde_json::to_string(&Self::from(r))
			.unwrap_or_else(|_|
				serde_json::to_string(&ResultFFI::<T>::Err(vec!("Unserializable".to_owned())))
				.unwrap()
			)
	}

	pub fn deserialize(s: &str) -> Result<T> where T: DeserializeOwned {
		serde_json::from_str::<Self>(s).with_context(|| {
			format!("Can't deserialize ResultFFI from:\n```\n{}\n```", s)
		})?.into_result()
	}
}

impl<T> From<Result<T>> for ResultFFI<T> {
	fn from(r: Result<T>) -> Self {
		match r {
			Result::Ok(t) => ResultFFI::Ok(t),
			Result::Err(e) => {
				ResultFFI::Err(e.chain().map(|c| c.to_string()).collect())
			},
		}
	}
}
