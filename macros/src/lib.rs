use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::parse_macro_input;

struct FnMatch<'a> {
	module_ident: &'a syn::Ident,
	fn_names: &'a Vec<String>,
}

impl <'a> ToTokens for FnMatch<'a> {
	fn to_tokens(&self, tokens: &mut TokenStream2) {
		for name in self.fn_names {
			let arm: syn::Arm = syn::parse_str(
				&format!("\"{}\" => {}::{}(c),\n", name, &self.module_ident, name)
			).expect(&format!("invalid generated match"));
			arm.to_tokens(tokens);
		}
	}
}

#[proc_macro_attribute]
pub fn export(attr: TokenStream, item: TokenStream) -> TokenStream {
	if !attr.is_empty() {
		panic!("can't pass an attribute to ambl_module attribute")
	}
	
	let mut orig = item.clone();

	// build up a list of functions. Anything accepting a single BaseCtx or TargetCtx argument gets exported.
	// Anything else will fail (if public), since that makes no sense
	let module = parse_macro_input!(item as syn::ItemMod);
	let mut base_fns: Vec<String> = vec!();
	let mut target_fns: Vec<String> = vec!();
	
	// extract public function names
	if let Some(content) = module.content {
		for item in content.1 {
			match item {
				syn::Item::Fn(fun) => {
					if let syn::Visibility::Public(_) = fun.vis {
						let fn_name = &fun.sig.ident;
						// eprintln!("Checking pub fn {}", &fn_name);
						let mut args = fun.sig.inputs.iter();
						if let Some(arg) = args.next() {
							if let syn::FnArg::Typed(typed) = arg {
								if let syn::Type::Path(ref typath) = *typed.ty {
									if let Some(last) = typath.path.segments.last() {
										match last.ident.to_string().as_str() {
											"BaseCtx" => {
												base_fns.push(fn_name.to_string());
											},
											"TargetCtx" => {
												target_fns.push(fn_name.to_string());
											},
											other => {
												panic!("Unexpected argument type {} in public function {}", other, fn_name);
											}
										}
									} else {
										panic!("Empty type path in {}", fn_name);
									}
								} else {
									panic!("Expected plain type argument in public function {}", fn_name);
								}
							} else {
								panic!("Unexpected `self` in public function {}", fn_name);
							}
						} else {
							panic!("Not enough arguments in public function {}", fn_name);
						}
						if args.next().is_some() {
							panic!("Too many args in public function {}", fn_name);
						}
					}
				},
				_ => ()
			}
		}
	}

	if base_fns.is_empty() && target_fns.is_empty() {
		panic!("No public functions found in module {}", &module.ident);
	}
	
	fn fn_match_lines<'a>(module_ident: &'a syn::Ident, fn_names: &'a Vec<String>) -> FnMatch<'a> {
		FnMatch { module_ident, fn_names }
	}
	
	let base_match_lines = fn_match_lines(&module.ident, &base_fns);
	let target_match_lines = fn_match_lines(&module.ident, &target_fns);

	// eprintln!("Base: {:?}", base_match_lines);
	// eprintln!("Target: {:?}", target_match_lines);

	// type WString = ::wit_bindgen::rt::string::String;
	let boilerplate: proc_macro2::TokenStream = quote::quote!{
		struct _BuilderImpl;
		impl ::ambl_api::Builder for _BuilderImpl {
			fn version() -> u8 {
				1
			}
			
			fn init(loglevel: u8) {
				::ambl_api::ambl_init(loglevel);
			}

			fn invoke(
				calltype: u8,
				symbol: ::wit_bindgen::rt::string::String,
				ctx: ::wit_bindgen::rt::string::String
			) -> ::wit_bindgen::rt::string::String {
				// ::log::debug!("invoking {} with ctx {}", &symbol, &ctx);
				match calltype {
					0 => {
						// rules
						::ambl_api::ResultFFI::<Vec<::ambl_api::Rule>>::serialize((||{
							let c: ::ambl_api::BaseCtx = ::ambl_api::serde_json::from_str(&ctx)?;
							match symbol.as_str() {
								#base_match_lines
								other => Err(::anyhow::anyhow!("Unknown rules function: {}", &other)),
							}
						})())
					},
					1 => {
						// target
						::ambl_api::ResultFFI::<()>::serialize((||{
							let c: ::ambl_api::TargetCtx = ::ambl_api::serde_json::from_str(&ctx)?;
							match symbol.as_str() {
								#target_match_lines
								other => Err(::ambl_api::anyhow::anyhow!("Unknown target function: {}", &other)),
							}
						})())
					},
					other => ::ambl_api::ResultFFI::<()>::serialize(Err(::ambl_api::anyhow::anyhow!("Unknown calltype: {}", &other)))
				}
			}
		}
		mod _ambl_api_export {
			use ambl_api::*;
			use super::_BuilderImpl;
			ambl_export_builder_raw!(_BuilderImpl);
		}
	};
	// eprintln!("Boilerplate: {}", &boilerplate);

	let boilerplate_tokens = TokenStream::from(boilerplate);
	orig.extend(boilerplate_tokens);
	orig
}
