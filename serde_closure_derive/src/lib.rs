//! Serializable and debuggable closures.
//!
//! <p style="font-family: 'Fira Sans',sans-serif;padding:0.3em 0"><strong>
//! <a href="https://crates.io/crates/serde_closure">📦&nbsp;&nbsp;Crates.io</a>&nbsp;&nbsp;│&nbsp;&nbsp;<a href="https://github.com/alecmocatta/serde_closure">📑&nbsp;&nbsp;GitHub</a>&nbsp;&nbsp;│&nbsp;&nbsp;<a href="https://constellation.zulipchat.com/#narrow/stream/213236-subprojects">💬&nbsp;&nbsp;Chat</a>
//! </strong></p>
//!
//! This library provides macros that wrap closures to make them serializable
//! and debuggable.
//!
//! See [`serde_closure`](https://docs.rs/serde_closure) for
//! documentation.

#![doc(html_root_url = "https://docs.rs/serde_closure_derive/0.3.3")]
#![allow(
	unknown_lints,
	clippy::default_trait_access,
	clippy::unused_self,
	clippy::if_not_else,
	clippy::manual_let_else
)]

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::HashSet, convert::TryInto, iter, iter::successors, mem::take, str};
use syn::{
	parse2, parse_macro_input, visit_mut::{self, VisitMut}, Arm, AttributeArgs, Block, Error, Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBlock, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprField, ExprForLoop, ExprGroup, ExprIf, ExprIndex, ExprLet, ExprLoop, ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprUnsafe, ExprWhile, ExprYield, FieldValue, GenericParam, Ident, ImplItem, Item, ItemImpl, Lifetime, LifetimeDef, Local, Member, Pat, PatBox, PatIdent, PatReference, PatSlice, PatTuple, PatTupleStruct, PatType, Path, PathArguments, PathSegment, ReturnType, Stmt, TraitBound, Type, TypeInfer, TypeReference, TypeTuple, UnOp
};

#[proc_macro]
#[allow(non_snake_case)]
pub fn Fn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	syn::parse::<ExprClosure>(input)
		.and_then(|closure| impl_closure(closure, Kind::Fn))
		.unwrap_or_else(|err| err.to_compile_error())
		.into()
}
#[proc_macro]
#[allow(non_snake_case)]
pub fn FnMut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	syn::parse::<ExprClosure>(input)
		.and_then(|closure| impl_closure(closure, Kind::FnMut))
		.unwrap_or_else(|err| err.to_compile_error())
		.into()
}
#[proc_macro]
#[allow(non_snake_case)]
pub fn FnOnce(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	syn::parse::<ExprClosure>(input)
		.and_then(|closure| impl_closure(closure, Kind::FnOnce))
		.unwrap_or_else(|err| err.to_compile_error())
		.into()
}

#[proc_macro_attribute]
pub fn desugar(
	attr: proc_macro::TokenStream, item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
	let args: AttributeArgs = parse_macro_input!(attr);
	assert_eq!(args.len(), 0);
	let mut item = match syn::parse::<Item>(item) {
		Err(err) => return err.to_compile_error().into(),
		Ok(item) => item,
	};
	Desugar.visit_item_mut(&mut item);
	item.into_token_stream().into()
}

struct Desugar;

impl Desugar {
	fn desugar_path_arg(
		&mut self, arg: &mut PathArguments, return_output: bool,
	) -> (u64, Option<Type>) {
		if let PathArguments::Parenthesized(args) = &arg {
			let span = Span::call_site();
			let mut lifetimes = 0;
			let mut inputs = args.inputs.clone();
			for input in &mut inputs {
				match input {
					Type::Reference(TypeReference { lifetime, .. }) if lifetime.is_none() => {
						*lifetime = Some(Lifetime::new(
							&format!(
								"'__serde_closure_{}",
								bijective_base(lifetimes, 26, alpha_lower)
							),
							span,
						));
						lifetimes += 1;
					}
					_ => (),
				}
			}
			if !inputs.empty_or_trailing() {
				inputs.push_punct(Default::default());
			}
			let output = match &args.output {
				ReturnType::Type(_, type_) => (**type_).clone(),
				ReturnType::Default => Type::Tuple(TypeTuple {
					paren_token: Default::default(),
					elems: Default::default(),
				}),
			};
			*arg = PathArguments::AngleBracketed(if !return_output {
				parse2(quote! { <(#inputs), Output = #output> }).unwrap()
			} else {
				parse2(quote! { <(#inputs)> }).unwrap()
			});
			(lifetimes, if return_output { Some(output) } else { None })
		} else {
			(0, None)
		}
	}
}

impl VisitMut for Desugar {
	fn visit_trait_bound_mut(&mut self, i: &mut TraitBound) {
		let lifetimes = self
			.desugar_path_arg(&mut i.path.segments.last_mut().unwrap().arguments, false)
			.0;
		if lifetimes > 0 {
			let span = Span::call_site();
			let empty = parse2(quote! {for <>}).unwrap();
			i.lifetimes = Some(i.lifetimes.clone().unwrap_or(empty));
			i.lifetimes
				.as_mut()
				.unwrap()
				.lifetimes
				.extend((0..lifetimes).map(|i| {
					LifetimeDef::new(Lifetime::new(
						&format!("'__serde_closure_{}", bijective_base(i, 26, alpha_lower)),
						span,
					))
				}));
		}
		visit_mut::visit_trait_bound_mut(self, i);
	}
	fn visit_item_impl_mut(&mut self, i: &mut ItemImpl) {
		if let Some((_, path, _)) = &mut i.trait_ {
			let (lifetimes, output) =
				self.desugar_path_arg(&mut path.segments.last_mut().unwrap().arguments, true);
			if lifetimes > 0 {
				let span = Span::call_site();
				i.generics.lt_token = Some(Default::default());
				i.generics.gt_token = Some(Default::default());
				i.generics.params.extend((0..lifetimes).map(|i| {
					GenericParam::Lifetime(LifetimeDef::new(Lifetime::new(
						&format!("'__serde_closure_{}", bijective_base(i, 26, alpha_lower)),
						span,
					)))
				}));
			}
			// Yuck
			if path.segments.last().unwrap().ident == "FnOnce" {
				if let Some(output) = output {
					i.items.push(ImplItem::Type(
						parse2(quote! { type Output = #output; }).unwrap(),
					));
				}
			}
		}
		visit_mut::visit_item_impl_mut(self, i);
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Kind {
	Fn,
	FnMut,
	FnOnce,
}
impl Kind {
	fn name(self) -> &'static str {
		match self {
			Kind::Fn => "Fn",
			Kind::FnMut => "FnMut",
			Kind::FnOnce => "FnOnce",
		}
	}
}

#[allow(
	clippy::cognitive_complexity,
	clippy::unnecessary_wraps,
	clippy::too_many_lines
)]
fn impl_closure(mut closure: ExprClosure, kind: Kind) -> Result<TokenStream, Error> {
	let span = Span::call_site(); // TODO: def_site() https://github.com/rust-lang/rust/issues/54724
	let name_string = kind.name();
	let name = Ident::new(name_string, span);
	let env_name = Ident::new("__serde_closure_env", span);
	let ret_name = Ident::new("__serde_closure_ret", span);
	let env_types_name = Ident::new("__serde_closure_env_types", span);
	let impls_name = Ident::new("__serde_closure_impls", span);

	let source = closure.to_token_stream().to_string();
	let capture = closure.capture.is_some();
	// Convert closure to use block so any not_env_variables can be asserted.
	closure.body = Box::new(match *closure.body {
		Expr::Block(block) => Expr::Block(block),
		// Avoid clippy::unused_unit
		Expr::Tuple(tuple) if tuple.elems.is_empty() => Expr::Tuple(tuple),
		expr => Expr::Block(ExprBlock {
			attrs: vec![],
			label: None,
			block: Block {
				brace_token: Default::default(),
				stmts: vec![Stmt::Expr(expr)],
			},
		}),
	});
	let mut closure = Expr::Closure(closure);
	let mut env_variables = HashSet::new();
	let mut not_env_variables = HashSet::new();
	State::new(
		&mut env_variables,
		&mut not_env_variables,
		kind != Kind::FnOnce,
		kind != Kind::FnOnce && !capture,
		&env_name,
	)
	.expr(&mut closure, false);
	assert!(not_env_variables.is_empty());
	let mut env_variables: Vec<Ident> = env_variables.into_iter().collect();
	env_variables.sort();
	let env_variables = &env_variables;
	let env_variable_names = &env_variables
		.iter()
		.map(ToString::to_string)
		.collect::<Vec<_>>();
	let closure = if let Expr::Closure(closure) = closure {
		closure
	} else {
		unreachable!()
	};

	let attrs = closure.attrs;
	let asyncness = closure.asyncness;
	let capture = closure.capture;
	let output = closure.output;
	let body = closure.body;
	// Avoid clippy::unused_unit
	let body = match *body {
		Expr::Tuple(tuple) if tuple.elems.is_empty() => None,
		expr => Some(expr),
	};
	let input_pats = closure.inputs.iter().map(|input| match input {
		Pat::Type(pat_type) => (*pat_type.pat).clone(),
		pat => (*pat).clone(),
	});
	let input_types = closure.inputs.iter().map(pat_to_type);

	let type_params = &(0..env_variables.len())
		.map(|i| {
			Ident::new(
				&format!(
					"__SERDE_CLOSURE_{}",
					bijective_base(i as u64, 26, alpha_upper)
				),
				span,
			)
		})
		.collect::<Vec<_>>();

	let type_params_infer = (0..env_variables.len()).map(|_| TypeInfer {
		underscore_token: Default::default(),
	});

	let ret_ref: Expr = parse2(match kind {
		Kind::Fn => quote! { &#ret_name },
		Kind::FnMut => quote! { &mut #ret_name },
		Kind::FnOnce => quote! { #ret_name },
	})
	.unwrap();
	let env_deref: Expr = parse2(match kind {
		Kind::Fn | Kind::FnMut => quote! { *#env_name },
		Kind::FnOnce => quote! { #env_name },
	})
	.unwrap();
	let env_type: Type = parse2(match kind {
		Kind::Fn => quote! { &#impls_name::#name<#(#type_params_infer,)*_> },
		Kind::FnMut => quote! { &mut #impls_name::#name<#(#type_params_infer,)*_> },
		Kind::FnOnce => quote! { #impls_name::#name<#(#type_params_infer,)*_> },
	})
	.unwrap();
	let env_capture = parse2::<ExprTuple>(match (kind, capture.is_some()) {
		(Kind::Fn, false) => quote! { ( #( & #env_variables ,)* ) },
		(Kind::FnMut, false) => quote! { ( #( &mut #env_variables ,)* ) },
		_ => quote! { ( #( #env_variables ,)* ) },
	})
	.unwrap()
	.elems;
	let env_deconstruct = if kind == Kind::FnOnce {
		Some(
			parse2::<Stmt>(
				quote! { #[allow(unused_mut)] let #impls_name::#name{#(mut #env_variables ,)*..} = #env_name; },
			)
			.unwrap(),
		)
	} else {
		None
	};

	let fn_impl = match kind {
		Kind::Fn => quote! {
			impl<#(#type_params,)* F, I, O> self::internal::FnOnce<I> for #name<#(#type_params,)* F>
			where
				F: ops::FnOnce(&#name<#(#type_params,)* ()>,I) -> O
			{
				type Output = O;

				#[inline(always)]
				fn call_once(self, args: I) -> Self::Output {
					self.f()(&self.strip_f(), args)
				}
			}
			impl<#(#type_params,)* F, I, O> self::internal::FnMut<I> for #name<#(#type_params,)* F>
			where
				F: ops::FnMut(&#name<#(#type_params,)* ()>,I) -> O
			{
				#[inline(always)]
				fn call_mut(&mut self, args: I) -> Self::Output {
					(&mut self.f())(self.strip_f_mut(), args)
				}
			}
			impl<#(#type_params,)* F, I, O> self::internal::Fn<I> for #name<#(#type_params,)* F>
			where
				F: ops::Fn(&#name<#(#type_params,)* ()>,I) -> O
			{
				#[inline(always)]
				fn call(&self, args: I) -> Self::Output {
					(&self.f())(self.strip_f_ref(), args)
				}
			}
		},
		Kind::FnMut => quote! {
			impl<#(#type_params,)* F, I, O> self::internal::FnOnce<I> for #name<#(#type_params,)* F>
			where
				F: ops::FnOnce(&mut #name<#(#type_params,)* ()>,I) -> O
			{
				type Output = O;

				#[inline(always)]
				fn call_once(mut self, args: I) -> Self::Output {
					self.f()(&mut self.strip_f(), args)
				}
			}
			impl<#(#type_params,)* F, I, O> self::internal::FnMut<I> for #name<#(#type_params,)* F>
			where
				F: ops::FnMut(&mut #name<#(#type_params,)* ()>,I) -> O
			{
				#[inline(always)]
				fn call_mut(&mut self, args: I) -> Self::Output {
					(&mut self.f())(self.strip_f_mut(), args)
				}
			}
		},
		Kind::FnOnce => quote! {
			impl<#(#type_params,)* F, I, O> self::internal::FnOnce<I> for #name<#(#type_params,)* F>
			where
				F: ops::FnOnce(#name<#(#type_params,)* ()>,I) -> O
			{
				type Output = O;

				#[inline(always)]
				fn call_once(self, args: I) -> Self::Output {
					self.f()(self.strip_f(), args)
				}
			}
		},
	};

	let serialize_bounds = quote! { #(#type_params: Serialize,)* }.to_string();
	let deserialize_bounds = quote! { #(#type_params: Deserialize<'de>,)* }.to_string();

	Ok(quote! {
		{
			mod #impls_name {
				#![allow(warnings)]
				use ::serde_closure::{
					internal::{self, is_phantom, to_phantom},
					structs,
				};
				use self::internal::core::{
					any::type_name,
					clone::Clone,
					cmp::{self, Eq, Ord, PartialEq, PartialOrd},
					convert::From,
					fmt::{self, Debug},
					hash::{self, Hash},
					marker::{Copy, PhantomData},
					mem::{self, size_of, MaybeUninit},
					ops,
					option::Option::{self, Some},
				};
				use self::internal::serde::{Deserialize, Serialize};
				use self::internal::std::{process::abort, string::{String, ToString}};
				const SOURCE: &str = #source;
				#[derive(Serialize, Deserialize)]
				#[serde(
					bound(serialize = #serialize_bounds),
					bound(deserialize = #deserialize_bounds)
				)]
				pub struct #name<#(#type_params,)* F> {
					#( pub #env_variables: #type_params, )*
					#[serde(skip)]
					__serde_closure_marker: PhantomData<F>
				}
				impl<#(#type_params,)*> #name<#(#type_params,)* ()> {
					pub fn new(#( #env_variables: #type_params, )*) -> Self {
						Self {
							#( #env_variables ,)*
							__serde_closure_marker: PhantomData
						}
					}
					pub fn with_f<F1>(self, f: F1) -> #name<#(#type_params,)* F1> where F1: Copy {
						if size_of::<F1>() != 0 {
							abort();
						}
						#name {
							#( #env_variables: self.#env_variables, )*
							__serde_closure_marker: PhantomData
						}
					}
				}
				impl<#(#type_params,)* F> #name<#(#type_params,)* F> {
					fn f(&self) -> F {
						// This is safe as an F has already been materialized (so we know it isn't
						// uninhabited), it's Copy, it's not Drop, its size is zero. Most
						// importantly, thanks to the `use Type` static assertion, we're guaranteed
						// not to be capturing anything other than `env_types_name`. Related:
						// https://internals.rust-lang.org/t/is-synthesizing-zero-sized-values-safe/11506
						unsafe { MaybeUninit::uninit().assume_init() }
					}
					// Strip F due to https://play.rust-lang.org/?edition=2018&gist=a2936c8b5abb13357d97bf835203b153
					// Another struct could hold env vars and avoid these unsafes but that slightly
					// increases complexity?
					fn strip_f(self) -> #name<#(#type_params,)* ()> {
						#name {
							#( #env_variables: self.#env_variables, )*
							__serde_closure_marker: PhantomData
						}
					}
					fn strip_f_ref(&self) -> &#name<#(#type_params,)* ()> {
						// This is safe as the size and alignment don't change
						unsafe { &*(self as *const _ as *const _) }
					}
					fn strip_f_mut(&mut self) -> &mut #name<#(#type_params,)* ()> {
						// This is safe as the size and alignment don't change
						unsafe { &mut *(self as *mut _ as *mut _) }
					}
				}
				impl<#(#type_params,)* F> Clone for #name<#(#type_params,)* F>
				where
					#(#type_params: Clone,)*
					F: Clone,
				{
					fn clone(&self) -> Self {
						Self {
							#( #env_variables: self.#env_variables.clone(), )*
							__serde_closure_marker: PhantomData,
						}
					}
				}
				impl<#(#type_params,)* F> Copy for #name<#(#type_params,)* F>
				where
					#(#type_params: Copy,)*
					F: Copy,
				{}
				impl<#(#type_params,)* F> PartialEq for #name<#(#type_params,)* F>
				where
					#(#type_params: PartialEq,)*
				{
					fn eq(&self, other: &Self) -> bool {
						#( self.#env_variables == self.#env_variables && )* true
					}
				}
				impl<#(#type_params,)* F> Eq for #name<#(#type_params,)* F>
				where
					#(#type_params: Eq,)*
				{}
				impl<#(#type_params,)* F> Hash for #name<#(#type_params,)* F>
				where
					#(#type_params: Hash,)*
				{
					fn hash<H: hash::Hasher>(&self, state: &mut H) {
						#( self.#env_variables.hash(state); )*
					}
				}
				impl<#(#type_params,)* F> PartialOrd for #name<#(#type_params,)* F>
				where
					#(#type_params: PartialOrd,)*
				{
					fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
						let mut ord = Some(cmp::Ordering::Equal);
						#(
							ord = if let Some(cmp::Ordering::Equal) = ord {
								self.#env_variables.partial_cmp(&other.#env_variables)
							} else { ord };
						)*
						ord
					}
				}
				impl<#(#type_params,)* F> Ord for #name<#(#type_params,)* F>
				where
					#(#type_params: Ord,)*
				{
					fn cmp(&self, other: &Self) -> cmp::Ordering {
						let mut ord = cmp::Ordering::Equal;
						#(
							ord = if let cmp::Ordering::Equal = ord {
								self.#env_variables.cmp(&other.#env_variables)
							} else { ord };
						)*
						ord
					}
				}
				impl<#(#type_params,)* F> Debug for #name<#(#type_params,)* F>
				where
					#(#type_params: Debug,)*
				{
					fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
						let mut name = String::from(#name_string);
						name.push('<');
						name.push_str(type_name::<F>());
						name.push_str(" at ");
						name.push_str(file!());
						name.push(':');
						name.push_str(&line!().to_string());
						name.push(':');
						name.push_str(&column!().to_string());
						name.push('>');
						f.debug_struct(&name)
							#( .field(#env_variable_names, &self.#env_variables) )*
							.field("source", &SOURCE)
							.finish()
					}
				}

				#fn_impl
			}

			// This asserts that inferred env variables aren't nameable types.
			#[allow(warnings)]
			{
				#(let #env_variables = ::serde_closure::internal::a_variable;)*
			}
			// This asserts that inferred env variables aren't types with >=1 type parameters.
			#[allow(warnings)]
			if false {
				#(&#env_variables::<>;)*
			}
			// TODO: Work out how to assert env variables aren't unnameable types with 0 type parameters.
			// This might work in the future, but today it causes borrowck issues:
			// https://users.rust-lang.org/t/statically-asserting-an-ident-is-a-variable-not-a-type/34619
			// #[allow(unreachable_code)]
			// {
			// 	if false {
			// 		#(#env_variables = loop {};)*
			// 	}
			// }

			#[allow(unused_mut)]
			let mut #ret_name = #impls_name::#name::new(#env_capture);
			let #env_types_name = ::serde_closure::internal::to_phantom(&#ret_name);

			let closure =
				#(#attrs)* #asyncness move |#env_name: #env_type, (#(#input_pats,)*): (#(#input_types,)*)| #output {
					#[allow(warnings)]
					if false {
						::serde_closure::internal::is_phantom(& #env_deref, #env_types_name);
					}
					#env_deconstruct
					#body
				};

			#[allow(warnings)]
			{
				if false {
					let _ = closure(#ret_ref, loop {});
				}
				if false {
					let ::serde_closure::internal::ZeroSizedAssertion = unsafe { ::serde_closure::internal::core::mem::transmute(closure) };
				}
			}

			::serde_closure::structs::#name::internal_new(#ret_name.with_f(closure))
		}
	})
}

fn pat_to_type(pat: &Pat) -> Type {
	match pat {
		Pat::Type(pat_type) => {
			if let Type::Infer(_) = *pat_type.ty {
				pat_to_type(&pat_type.pat)
			} else {
				(*pat_type.ty).clone()
			}
		}
		Pat::Reference(PatReference {
			and_token,
			mutability,
			pat,
			..
		}) => Type::Reference(TypeReference {
			and_token: *and_token,
			lifetime: None,
			mutability: *mutability,
			elem: Box::new(pat_to_type(pat)),
		}),
		_ => Type::Infer(TypeInfer {
			underscore_token: Default::default(),
		}),
	}
}

struct State<'a> {
	variables: HashSet<Ident>,
	env_variables: &'a mut HashSet<Ident>,
	not_env_variables: &'a mut HashSet<Ident>,
	env_struct: bool,
	deref: bool,
	env_name: &'a Ident,
}
impl<'a> State<'a> {
	fn new(
		env_variables: &'a mut HashSet<Ident>, not_env_variables: &'a mut HashSet<Ident>,
		env_struct: bool, deref: bool, env_name: &'a Ident,
	) -> Self {
		Self {
			variables: HashSet::new(),
			env_variables,
			not_env_variables,
			env_struct,
			deref,
			env_name,
		}
	}
	fn enter_block(&mut self) -> State<'_> {
		State {
			variables: self.variables.clone(),
			env_variables: self.env_variables,
			not_env_variables: self.not_env_variables,
			env_struct: self.env_struct,
			deref: self.deref,
			env_name: self.env_name,
		}
	}
	fn pat(&mut self, pat: &mut Pat) {
		match pat {
			Pat::Ident(PatIdent { ident, subpat, .. }) => {
				// declaring a variable
				assert_ne!(ident, self.env_name);
				let _ = self.variables.insert(ident.clone());
				if let Some((_, subpat)) = subpat {
					self.pat(subpat);
				}
			}
			Pat::Box(PatBox { pat, .. })
			| Pat::Reference(PatReference { pat, .. })
			| Pat::Type(PatType { pat, .. }) => self.pat(&mut *pat),
			Pat::Or(pat_or) => {
				for case in &mut pat_or.cases {
					self.pat(case);
				}
			}
			Pat::Slice(PatSlice { elems, .. })
			| Pat::Tuple(PatTuple { elems, .. })
			| Pat::TupleStruct(PatTupleStruct {
				pat: PatTuple { elems, .. },
				..
			}) => {
				for elem in elems {
					self.pat(elem);
				}
			}
			Pat::Range(pat_range) => {
				self.expr(&mut pat_range.lo, false);
				self.expr(&mut pat_range.hi, false);
			}
			Pat::Struct(pat_struct) => {
				for field in &mut pat_struct.fields {
					self.pat(&mut field.pat);
				}
			}
			_ => (),
		}
	}

	fn block(&mut self, stmts: &mut Vec<Stmt>) {
		*stmts = take(stmts)
			.into_iter()
			.flat_map(|mut stmt| {
				match &mut stmt {
					Stmt::Local(Local { pat, init, .. }) => {
						if let Some((_, expr)) = init {
							self.expr(expr, false);
						}
						self.pat(pat);
					}
					Stmt::Expr(expr) | Stmt::Semi(expr, _) => {
						self.expr(expr, false);
					}
					Stmt::Item(_) => (),
				}
				let not_env_variables = take(self.not_env_variables).into_iter();
				let mut vec = Vec::with_capacity(2);
				if not_env_variables.len() != 0 {
					vec.push(
						parse2(quote! { #[allow(warnings)] { #(use #not_env_variables;)* } })
							.unwrap(),
					);
				}
				vec.push(stmt);
				vec
			})
			.collect();
	}

	#[allow(clippy::too_many_lines)]
	fn expr(&mut self, expr: &mut Expr, is_func: bool) {
		match expr {
			Expr::Array(ExprArray { elems, .. }) | Expr::Tuple(ExprTuple { elems, .. }) => {
				for elem in elems {
					self.expr(elem, false);
				}
			}
			Expr::Assign(ExprAssign { left, right, .. })
			| Expr::AssignOp(ExprAssignOp { left, right, .. })
			| Expr::Binary(ExprBinary { left, right, .. })
			| Expr::Index(ExprIndex {
				expr: left,
				index: right,
				..
			})
			| Expr::Repeat(ExprRepeat {
				expr: left,
				len: right,
				..
			}) => {
				self.expr(left, false);
				self.expr(right, false);
			}
			Expr::Async(ExprAsync { block, .. })
			| Expr::Block(ExprBlock { block, .. })
			| Expr::Loop(ExprLoop { body: block, .. })
			| Expr::TryBlock(ExprTryBlock { block, .. })
			| Expr::Unsafe(ExprUnsafe { block, .. }) => {
				self.enter_block().block(&mut block.stmts);
			}
			Expr::Await(ExprAwait { base: expr, .. })
			| Expr::Box(ExprBox { expr, .. })
			| Expr::Break(ExprBreak {
				expr: Some(expr), ..
			})
			| Expr::Cast(ExprCast { expr, .. })
			| Expr::Field(ExprField { base: expr, .. })
			| Expr::Group(ExprGroup { expr, .. })
			| Expr::Paren(ExprParen { expr, .. })
			| Expr::Reference(ExprReference { expr, .. })
			| Expr::Return(ExprReturn {
				expr: Some(expr), ..
			})
			| Expr::Try(ExprTry { expr, .. })
			| Expr::Type(ExprType { expr, .. })
			| Expr::Unary(ExprUnary { expr, .. })
			| Expr::Yield(ExprYield {
				expr: Some(expr), ..
			}) => {
				self.expr(expr, false);
			}
			Expr::Call(ExprCall { func, args, .. }) => {
				match &**func {
					Expr::Path(ExprPath { path, .. })
						if path.leading_colon.is_none() && path.segments.len() == 1 =>
					{
						self.expr(func, true);
					}
					_ => self.expr(func, false),
				}
				for arg in args {
					self.expr(arg, false);
				}
			}
			Expr::Closure(ExprClosure { inputs, body, .. }) => {
				let mut state = self.enter_block();
				for input in inputs {
					state.pat(input);
				}
				state.expr(body, false);
			}
			Expr::ForLoop(ExprForLoop {
				pat, expr, body, ..
			}) => {
				self.expr(expr, false);
				let mut state = self.enter_block();
				state.pat(pat);
				state.block(&mut body.stmts);
			}
			Expr::If(ExprIf {
				cond,
				then_branch,
				else_branch,
				..
			}) => {
				{
					let mut state = self.enter_block();
					state.expr(cond, false);
					state.block(&mut then_branch.stmts);
				}
				if let Some((_, else_branch)) = else_branch {
					self.expr(else_branch, false);
				}
			}
			Expr::Let(ExprLet { pat, expr, .. }) => {
				self.expr(expr, false);
				self.pat(pat);
			}
			Expr::Match(ExprMatch { expr, arms, .. }) => {
				self.expr(expr, false);
				for Arm {
					pat, guard, body, ..
				} in arms
				{
					let mut state = self.enter_block();
					state.pat(pat);
					if let Some((_, guard)) = guard {
						state.expr(guard, false);
					}
					state.expr(body, false);
				}
			}
			Expr::MethodCall(ExprMethodCall { receiver, args, .. }) => {
				self.expr(receiver, false);
				for arg in args {
					self.expr(arg, false);
				}
			}
			Expr::Path(ExprPath { attrs, path, .. })
				if path.leading_colon.is_none() && path.segments.len() == 1 =>
			{
				let path_segment = &path.segments.first().unwrap();
				let ident = &path_segment.ident;
				let has_path_arguments = !matches!(path_segment.arguments, PathArguments::None);
				if !self.variables.contains(ident) {
					// Assume it's a variable, unless:
					// * It starts with an upper-case letter, e.g. `Some`, OR
					// * It's a function call, e.g. `my_function()`, OR
					// * It has a turbofish, e.g. `my_function::<T>`
					if !(ident.to_string().chars().next().unwrap().is_uppercase()
						|| is_func || has_path_arguments)
					{
						let _ = self.env_variables.insert(ident.clone());
						if self.env_struct {
							let mut a = Expr::Field(ExprField {
								attrs: vec![],
								base: Box::new(Expr::Path(ExprPath {
									attrs: attrs.clone(),
									qself: None,
									path: Path {
										leading_colon: None,
										segments: iter::once(PathSegment {
											ident: self.env_name.clone(),
											arguments: PathArguments::None,
										})
										.collect(),
									},
								})),
								dot_token: Default::default(),
								member: Member::Named(ident.clone()),
							});
							if self.deref {
								a = Expr::Unary(ExprUnary {
									attrs: vec![],
									op: UnOp::Deref(Default::default()),
									expr: Box::new(a),
								});
							}
							*expr = Expr::Paren(ExprParen {
								attrs: vec![],
								paren_token: Default::default(),
								expr: Box::new(a),
							});
						}
					} else {
						let _ = self.not_env_variables.insert(ident.clone());
					}
				}
			}
			Expr::Range(ExprRange { from, to, .. }) => {
				if let Some(from) = from {
					self.expr(from, false);
				}
				if let Some(to) = to {
					self.expr(to, false);
				}
			}
			Expr::Struct(ExprStruct { fields, rest, .. }) => {
				for FieldValue { expr, .. } in fields {
					self.expr(expr, false);
				}
				if let Some(rest) = rest {
					self.expr(rest, false);
				}
			}
			Expr::While(ExprWhile { cond, body, .. }) => {
				let mut state = self.enter_block();
				state.expr(cond, false);
				state.block(&mut body.stmts);
			}
			Expr::Macro(ExprMacro { ref mut mac, .. }) => {
				let tokens = &mac.tokens;
				if let Ok(expr) = parse2::<ExprCall>(quote! { self::abc(#tokens) }) {
					let mut expr = Expr::Call(expr);
					self.expr(&mut expr, false);
					let expr = if let Expr::Call(expr) = expr {
						expr
					} else {
						unreachable!()
					};
					mac.tokens = expr.args.to_token_stream();
				} else {
					panic!("See https://github.com/alecmocatta/serde_closure/issues/16");
					// proc_macro_diagnostic: https://github.com/rust-lang/rust/issues/54140
					// mac.span()
					// 	.unwrap()
					// 	.warning("See https://github.com/alecmocatta/serde_closure/issues/16")
					// 	.emit()
				}
			}
			_ => (),
		}
	}
}

#[inline(always)]
fn alpha_upper(u: u8) -> u8 {
	assert!(u < 26);
	u + b'A'
}
#[inline(always)]
fn alpha_lower(u: u8) -> u8 {
	assert!(u < 26);
	u + b'a'
}
const BUF_SIZE: usize = 64; // u64::max_value() in base 2
fn bijective_base(n: u64, base: u64, digits: impl Fn(u8) -> u8) -> String {
	let mut buffer = [0_u8; BUF_SIZE];
	let divided = successors(Some(n), |n| match n / base {
		0 => None,
		n => Some(n - 1),
	});
	#[allow(clippy::suspicious_map)]
	let written = buffer
		.iter_mut()
		.rev()
		.zip(divided)
		.map(|(c, n)| *c = digits((n % base).try_into().unwrap()))
		.count();
	let index = BUF_SIZE - written;

	str::from_utf8(&buffer[index..]).unwrap().to_owned()
}

#[test]
fn bijective_base_test() {
	for i in 0..=26 + 26 * 26 + 26 * 26 * 26 {
		let _ = bijective_base(i, 26, alpha_upper);
	}
	assert_eq!(
		bijective_base(26 + 26 * 26 + 26 * 26 * 26, 26, alpha_upper),
		"AAAA"
	);
	assert_eq!(
		bijective_base(u64::max_value(), 3, alpha_upper),
		"AAAABBABCBBABBAABCCAACCCACAACACCACCBAABBA"
	);
	assert_eq!(
		bijective_base(u64::max_value(), 2, alpha_upper),
		"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"
	);
}
