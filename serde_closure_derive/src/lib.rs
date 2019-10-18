//! Serializable and debuggable closures.
//!
//! **[Crates.io](https://crates.io/crates/serde_closure) │
//! [Repo](https://github.com/alecmocatta/serde_closure)**
//!
//! This library provides macros that wrap closures to make them serializable
//! and debuggable.
//!
//! See [`serde_closure`](https://docs.rs/serde_closure/) for
//! documentation.

#![doc(html_root_url = "https://docs.rs/serde_closure_derive/0.2.4")]
#![feature(proc_macro_diagnostic)]
#![allow(non_snake_case)] // due to proc-macro-hack can't apply this directly

extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use proc_macro_hack::proc_macro_hack;
use quote::{quote, quote_spanned, ToTokens};
use std::{collections::HashSet, iter, iter::successors, str};
use syn::{
	parse::{Parse, ParseStream}, parse2, spanned::Spanned, token::Bracket, Arm, Error, Expr, ExprArray, ExprAssign, ExprAssignOp, ExprAsync, ExprAwait, ExprBinary, ExprBlock, ExprBox, ExprBreak, ExprCall, ExprCast, ExprClosure, ExprField, ExprForLoop, ExprGroup, ExprIf, ExprIndex, ExprLet, ExprLoop, ExprMacro, ExprMatch, ExprMethodCall, ExprParen, ExprPath, ExprRange, ExprReference, ExprRepeat, ExprReturn, ExprStruct, ExprTry, ExprTryBlock, ExprTuple, ExprType, ExprUnary, ExprUnsafe, ExprWhile, ExprYield, FieldValue, Ident, Local, Member, Pat, PatBox, PatIdent, PatReference, PatSlice, PatTuple, PatTupleStruct, PatType, Path, PathArguments, PathSegment, Stmt, Type, TypeInfer, TypeReference, UnOp
};

#[proc_macro_hack]
pub fn Fn(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	syn::parse::<Closure>(input)
		.and_then(|closure| impl_fn_once(closure, Kind::Fn))
		.unwrap_or_else(|err| err.to_compile_error())
		.into()
}
#[proc_macro_hack]
pub fn FnMut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	syn::parse::<Closure>(input)
		.and_then(|closure| impl_fn_once(closure, Kind::FnMut))
		.unwrap_or_else(|err| err.to_compile_error())
		.into()
}
#[proc_macro_hack]
pub fn FnOnce(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	syn::parse::<Closure>(input)
		.and_then(|closure| impl_fn_once(closure, Kind::FnOnce))
		.unwrap_or_else(|err| err.to_compile_error())
		.into()
}

struct Closure {
	env: Option<ExprArray>,
	closure: ExprClosure,
}
impl Parse for Closure {
	fn parse(input: ParseStream) -> Result<Self, Error> {
		let env = if input.peek(Bracket) {
			Some(input.parse()?)
		} else {
			None
		};
		let closure = input.parse()?;
		Ok(Closure { env, closure })
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

#[allow(clippy::cognitive_complexity)]
fn impl_fn_once(closure: Closure, kind: Kind) -> Result<TokenStream, Error> {
	let span = Span::call_site(); // TODO: def_site() https://github.com/rust-lang/rust/issues/54724
	let name_string = kind.name();
	let name = Ident::new(name_string, span);
	let env_name = Ident::new("__serde_closure_env", span);
	let ret_name = Ident::new("__serde_closure_ret", span);
	let env_types_name = Ident::new("__serde_closure_env_types", span);
	let impls_name = Ident::new("__serde_closure_impls", span);

	let _ = closure.env;
	let closure = closure.closure;
	let source = closure.to_token_stream().to_string();
	let capture = closure.capture.is_some();
	let mut closure = Expr::Closure(closure);
	let mut env_variables = HashSet::new();
	State::new(
		&mut env_variables,
		kind != Kind::FnOnce,
		kind != Kind::FnOnce && !capture,
		&env_name,
	)
	.expr(&mut closure, false);
	let mut env_variables: Vec<Ident> = env_variables.into_iter().collect();
	env_variables.sort();
	let env_variables = &env_variables;
	let env_variable_names = &env_variables
		.iter()
		.map(|ident| ident.to_string())
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
	let input_pats = closure.inputs.iter().map(|input| match input {
		Pat::Type(pat_type) => (*pat_type.pat).clone(),
		pat => (*pat).clone(),
	});
	let input_types = closure.inputs.iter().map(pat_to_type);
	// let line_number = format!(" {}:{}:{}", closure.span().source_file(), closure.span().start().line, closure.span().start().column);

	let type_params = &(0..env_variables.len())
		.map(|i| {
			Ident::new(
				&format!("__SERDE_CLOSURE_{}", bijective_base(i as u64, 26, alpha)),
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
		Kind::Fn => quote! { *#env_name },
		Kind::FnMut => quote! { *#env_name },
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
				quote! { let #impls_name::#name{#(mut #env_variables ,)*..} = #env_name; },
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
					f: PhantomData<F>
				}
				impl<#(#type_params,)*> #name<#(#type_params,)* ()> {
					pub fn new(#( #env_variables: #type_params, )*) -> Self {
						Self {
							#( #env_variables ,)*
							f: PhantomData
						}
					}
					pub fn with_f<F1>(self, f: F1) -> #name<#(#type_params,)* F1> where F1: Copy {
						if size_of::<F1>() != 0 {
							abort();
						}
						#name {
							#( #env_variables: self.#env_variables, )*
							f: PhantomData
						}
					}
				}
				impl<#(#type_params,)* F> #name<#(#type_params,)* F> {
					fn f(&self) -> F {
						// This is safe as an F has already been materialized (so we
						// know it isn't uninhabited), it's Copy, it's not Drop, and
						// its size is zero.
						unsafe { MaybeUninit::uninit().assume_init() }
					}
					fn strip_f(self) -> #name<#(#type_params,)* ()> {
						#name {
							#( #env_variables: self.#env_variables, )*
							f: PhantomData
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
							f: PhantomData,
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

			let mut #ret_name = #impls_name::#name::new(#env_capture);
			let #env_types_name = ::serde_closure::internal::to_phantom(&#ret_name);

			let closure =
				#(#attrs)* #asyncness move |mut #env_name: #env_type, (#(#input_pats,)*): (#(#input_types,)*)| #output {
					if false {
						::serde_closure::internal::is_phantom(& #env_deref, #env_types_name);
					}
					#env_deconstruct
					#body
				};

			if false {
				#[allow(unreachable_code)]
				let _ = closure(#ret_ref, loop {});
			}
			if false {
				let ::serde_closure::internal::ZeroSizedAssertion = unsafe { ::serde_closure::internal::core::mem::transmute(closure) };
			}

			::serde_closure::structs::#name::internal_new(#ret_name.with_f(closure))
		}
	})
}

fn pat_to_type(pat: &Pat) -> Type {
	match pat {
		Pat::Type(pat_type) => {
			if let Type::Infer(_) = *pat_type.ty {
				pat_to_type(&*pat_type.pat)
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
	env_struct: bool,
	deref: bool,
	env_name: &'a Ident,
}
impl<'a> State<'a> {
	fn new(
		env_variables: &'a mut HashSet<Ident>, env_struct: bool, deref: bool, env_name: &'a Ident,
	) -> Self {
		Self {
			variables: HashSet::new(),
			env_variables,
			env_struct,
			deref,
			env_name,
		}
	}
	fn enter_block(&mut self) -> State<'_> {
		State {
			variables: self.variables.clone(),
			env_variables: self.env_variables,
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
					self.pat(case)
				}
			}
			Pat::Slice(PatSlice { elems, .. })
			| Pat::Tuple(PatTuple { elems, .. })
			| Pat::TupleStruct(PatTupleStruct {
				pat: PatTuple { elems, .. },
				..
			}) => {
				for elem in elems {
					self.pat(elem)
				}
			}
			Pat::Range(pat_range) => {
				self.expr(&mut pat_range.lo, false);
				self.expr(&mut pat_range.hi, false);
			}
			Pat::Struct(pat_struct) => {
				for field in &mut pat_struct.fields {
					self.pat(&mut *field.pat)
				}
			}
			_ => (),
		}
	}

	fn block(&mut self, stmts: &mut [Stmt]) {
		for stmt in stmts {
			match stmt {
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
		}
	}

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
						self.expr(func, true)
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
				let has_path_arguments = if let PathArguments::None = path_segment.arguments {
					false
				} else {
					true
				};
				if !self.variables.contains(ident) {
					// Assume it's a variable, unless:
					// * It starts with an upper-case letter, e.g. `Some`, OR
					// * It's a function call, e.g. `my_function()`, OR
					// * It has a turbofish, e.g. `my_function::<T>`
					if !(ident.to_string().chars().next().unwrap().is_uppercase()
						|| is_func || has_path_arguments)
					{
						let _ = self.env_variables.insert(ident.clone());
						let mut a = if !self.env_struct {
							Expr::Path(ExprPath {
								attrs: attrs.clone(),
								qself: None,
								path: path.clone(),
							})
						} else {
							Expr::Field(ExprField {
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
							})
						};
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
					} else {
						*expr = parse2(quote_spanned!{ expr.span() =>
							({
								let x = #expr;
								if false {
									let ::serde_closure::internal::ZeroSizedAssertion = unsafe { ::serde_closure::internal::core::mem::transmute(::serde_closure::internal::core::ptr::read(&x)) };
								}
								x
							})
						}).unwrap();
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
					mac.span()
						.unwrap()
						.warning("See https://github.com/alecmocatta/serde_closure/issues/16")
						.emit()
				}
			}
			_ => (),
		}
	}
}

#[inline(always)]
fn alpha(u: u8) -> u8 {
	assert!(u < 26);
	u + b'A'
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
		.map(|(c, n)| *c = digits((n % base) as u8))
		.count();
	let index = BUF_SIZE - written;

	str::from_utf8(&buffer[index..]).unwrap().to_owned()
}

#[test]
fn bijective_base_test() {
	for i in 0..=26 + 26 * 26 + 26 * 26 * 26 {
		let _ = bijective_base(i, 26, alpha);
	}
	assert_eq!(
		bijective_base(26 + 26 * 26 + 26 * 26 * 26, 26, alpha),
		"AAAA"
	);
	assert_eq!(
		bijective_base(u64::max_value(), 3, alpha),
		"AAAABBABCBBABBAABCCAACCCACAACACCACCBAABBA"
	);
	assert_eq!(
		bijective_base(u64::max_value(), 2, alpha),
		"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB"
	);
}
