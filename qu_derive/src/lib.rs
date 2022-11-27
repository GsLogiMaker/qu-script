

extern crate proc_macro;
extern crate syn;
extern crate quote;


use std::mem::take;

use proc_macro::TokenStream;
use quote::ToTokens;
use quote::format_ident;
use quote::quote;
use syn::ExprTuple;
use syn::FnArg;
use syn::Ident;
use syn::ImplItem;
use syn::ImplItemMethod;
use syn::ItemImpl;
use syn::ReturnType;
use syn::Visibility;
use syn::parse;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::__private::TokenStream2 as SynTokenStream;


#[derive(Debug)]
struct WrapperData {
	stream:SynTokenStream,
	name:Ident,
	arg_types:Vec<String>,
	return_type:Option<String>,
	fn_ptr:SynTokenStream,
}


fn get_fns_of_impl_body(impl_block:&ItemImpl) -> Vec<&ImplItemMethod> {
	let mut methods = Vec::default();
	for item in &impl_block.items {
		// Item is a method
		let ImplItem::Method(method) = item else {
			continue
		};
		// Method is public
		let Visibility::Public(_) = method.vis else {
			continue
		};
		// Method references self
		let Some(FnArg::Receiver(_)) = method.sig.inputs.first() else {
			continue
		};
		// Method does not return void
		let ReturnType::Type(_, _) = method.sig.output else {
			continue
		};
		methods.push(method)
	}
	methods
}


fn get_static_fns_of_impl_body(impl_block:&ItemImpl) -> Vec<&ImplItemMethod> {
	let mut methods = Vec::default();
	for item in &impl_block.items {
		// Item is a method
		let ImplItem::Method(method) = item else {
			continue
		};
		// Method is public
		let Visibility::Public(_) = method.vis else {
			continue
		};
		let first_arg = method.sig.inputs.first();
		// Method does not reference self
		match first_arg {
			Some(FnArg::Receiver(_)) => continue,
			_ => {/* pass */},
		};
		// Method does not return void
		let ReturnType::Type(_, _) = method.sig.output else {
			continue
		};
		methods.push(method)
	}
	methods
}


fn get_void_fns_of_impl_body(impl_block:&ItemImpl) -> Vec<&ImplItemMethod> {
	let mut methods = Vec::default();
	for item in &impl_block.items {
		// Item is a method
		let ImplItem::Method(method) = item else {
			continue
		};
		// Method is public
		let Visibility::Public(_) = method.vis else {
			continue
		};
		// Method references self
		let Some(FnArg::Receiver(_)) = method.sig.inputs.first() else {
			continue
		};
		// Method returns void
		let ReturnType::Default = method.sig.output else {
			continue
		};
		methods.push(method)
	}
	methods
}


fn define_fn_wrappers(
	impl_block:&ItemImpl,
	fns:&Vec<&ImplItemMethod>,
	static_fns:&Vec<&ImplItemMethod>,
	void_fns:&Vec<&ImplItemMethod>,
) -> (Vec<ImplItem>, ItemImpl) {

	let type_name = &impl_block.self_ty.to_token_stream().to_string();
	let methods = fns
		.iter()
		.chain(void_fns)
		.chain(static_fns);

	// Define wrapper functions
	let mut fn_wrappers:Vec<WrapperData> = Vec::default();
	let mut static_wrappers:Vec<WrapperData> = Vec::default();
	let mut void_wrappers:Vec<WrapperData> = Vec::default();
	for m in methods {
		let fn_sig = &m.sig;
		let fn_name = &fn_sig.ident;
		let fn_args = &fn_sig.inputs;
		let fn_arg_count = fn_args.len();
		let fn_return_ty = &fn_sig.output;

		// Handle function arguments
		let mut arg_types = Vec::default();
		let mut stack_getters = Vec::default();
		let mut arg_vars = Vec::default();
		let mut i = 0usize;
		for fn_arg in fn_args{
			match fn_arg {
				FnArg::Receiver(_) => {
					stack_getters.push(quote!(
						let source = vm.reg_get_as::<Self>(args[#i])?;
					));
				},
				FnArg::Typed(pat_type) => {
					let arg_type = &*pat_type.ty;
					let arg_var = format_ident!("p{i}");
					stack_getters.push(quote!(
						let #arg_var = vm.reg_get_as::<#arg_type>(args[#i])?;
					));
					arg_vars.push(quote!(*#arg_var));
					i += 1;
					
				},
			};
			arg_types.push(fn_arg.to_token_stream().to_string());
		}

		// Handle static methods
		let fn_caller = match fn_is_static(m) {
			true => quote!(Self::),
			false => quote!(source.),
		};

		// Handle returning void
		let (
			return_output,
			return_type,
		) = match fn_sig.output {
			ReturnType::Type(_,_) => (
				quote!(Ok(Box::new(output))),
				quote!(Result<RegisterValue, QuMsg>),
			),
			ReturnType::Default => (
				quote!(Ok(())),
				quote!(Result<(), QuMsg>),
			),
		};

		// Make function wrapper name
		let wrapper_fn_name = format_ident!("quwrapper{fn_name}");

		// Make wrapper function
		let stream = quote!(
			pub fn #wrapper_fn_name (
				vm:&mut QuVm,
				args:Vec<QuRegId>,
			) -> #return_type {
				if args.len() != #fn_arg_count {
					return Err(QuMsg::general("incorrect argument quantity"));
				}

				#(#stack_getters)*
		
				let output = #fn_caller #fn_name( #(#arg_vars),* );
				#return_output
			}
		);

		let fn_ptr = match fn_is_void(&m) {
			true => quote!(QuFnVoidPtr(&Self::#wrapper_fn_name)),
			false => quote!(QuFnPtr(&Self::#wrapper_fn_name)),
		};
		let return_type = match fn_is_void(m) {
			true => None,
			false => Some(fn_return_ty.to_token_stream().to_string()),
		};
		let data = WrapperData{
			stream,
			name: wrapper_fn_name.clone(),
			arg_types,
			return_type,
			fn_ptr,
		};

		if fn_is_static(&m) {
			static_wrappers.push(data);
		} else if fn_is_void(&m) {
			void_wrappers.push(data);
		} else {
			fn_wrappers.push(data);
		}
	}

	// Implement QuRegisterStruct
	let fn_regdata_items = make_wrapper_datas(&fn_wrappers);
	let static_regdata_items = make_wrapper_datas(&static_wrappers);
	let void_regdata_items = make_wrapper_datas(&void_wrappers);

	let mut wrapper_items = Vec::default();
	for d in fn_wrappers.iter_mut()
		.chain(static_wrappers.iter_mut())
		.chain(void_wrappers.iter_mut())
	{
		let stream = TokenStream::from(take(&mut d.stream));
		let Ok(p) = parse::<ImplItemMethod>(stream)
			else {panic!()};
		wrapper_items.push(ImplItem::Method(p));
	}

	let impl_register = parse_quote!(
		impl QuRegisterStruct for QuInt {

			fn register_fns() -> Vec<QuExtFnData> {
				return vec![
					#(#fn_regdata_items),*
				];
			}
		
			fn register_static_fns() -> Vec<QuExtFnData> {
				return vec![
					#(#static_regdata_items),*
				];
			}
		
		
			fn register_void_fns() -> Vec<QuExtVoidFnData> {
				return vec![
					#(#void_regdata_items),*
				];
			}
		
		
			fn get_name() -> &'static str {
				& #type_name
			}
		}
	);
	
	

	(wrapper_items, impl_register)

}


fn make_wrapper_datas(functions:&Vec<WrapperData>) -> Vec<ExprTuple> {
	let mut fn_datas = Vec::default();

	for f in functions {
		let fn_name_tk = &f.name;
		let fn_name_str = fn_name_tk.to_string();
		let fn_ref_data = &f.fn_ptr;
		let args = &f.arg_types;
		let return_ty = match &f.return_type {
			Some(r) => quote!(#r,),
			None => quote!(),
		};

		let tuple_item:ExprTuple = parse_quote!(
			(
				#fn_name_str.to_owned(),
				#fn_ref_data,
				vec![#(#args),*],
				#return_ty
			)
		);
		fn_datas.push(tuple_item);
	}

	fn_datas
}


fn fn_is_static(fn_item:&ImplItemMethod) -> bool {
	let fn_sig = &fn_item.sig;
	let Some(first_arg) = fn_sig.inputs.first() else {
		// Has no arguments, return true
		return true;
	};

	if let FnArg::Receiver(_) = first_arg {
		// First argument is self. Return false
		return false;
	}

	// First argument is NOT self. Return true
	return true;
}


fn fn_is_void(fn_item:&ImplItemMethod) -> bool {
	let fn_sig = &fn_item.sig;
	return match fn_sig.output {
		ReturnType::Type(_, _) => false,
		ReturnType::Default => true,
	};
}


#[proc_macro_attribute]
pub fn qu_methods(_attr: TokenStream, item: TokenStream) -> TokenStream {
	let mut impl_block = parse_macro_input!(item as ItemImpl);

	let fns = get_fns_of_impl_body(&impl_block);
	let static_fns = get_static_fns_of_impl_body(&impl_block);
	let void_fns = get_void_fns_of_impl_body(&impl_block);

	let (
		mut wrappers,
		impl_register,
	) = define_fn_wrappers(
		&impl_block,
		&fns,
		&static_fns,
		&void_fns,
	);

	impl_block.items.append(&mut wrappers);

	let mut stream = TokenStream::default();
	stream.extend(TokenStream::from(impl_block.to_token_stream()));
	stream.extend(TokenStream::from(impl_register.to_token_stream()));
	stream
	
}


#[proc_macro_attribute]
pub fn qu_fn(_: TokenStream, _: TokenStream) -> TokenStream {
	// TODO: Panic if not applied to a function
	"".parse().unwrap()
}