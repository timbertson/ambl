use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn export(_attr: TokenStream, item: TokenStream) -> TokenStream {
	let orig = item.clone();
	let f = parse_macro_input!(item as syn::ItemFn);
	let fn_name = &f.sig.ident;
	let ffi_name = Ident::new(&format!("amblffi_{}", &fn_name), Span::call_site());
	let output: proc_macro2::TokenStream = quote::quote!{
		#[no_mangle]
		pub extern "C" fn #ffi_name<'a>(ptr_in: *const u8, len_in: u32, ptr_out: &'a mut *mut u8, len_out: &'a mut u32) {{
			::ambl_api::wrap_fn_mut1(#fn_name, ptr_in, len_in, ptr_out, len_out)
		}}
	};

	let mut wrapper = TokenStream::from(output);
	// keep original definition
	wrapper.extend(orig);
	wrapper
}
