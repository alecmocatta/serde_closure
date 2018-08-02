//! Serializable closures.
//!
//! **[Crates.io](https://crates.io/crates/serde_closure) │
//! [Repo](https://github.com/alecmocatta/serde_closure)**
//!
//! This library provides macros to wrap closures such that they can be
//! serialized and sent between other processes running the same binary.
//!
//! ```
//! # #![feature(never_type)]
//! # #[macro_use] extern crate serde_closure;
//! # use std::{iter};
//! # trait IntoDistributedIterator { //where for <'a> &'a Self: IntoDistributedIterator, for <'a> &'a mut Self: IntoDistributedIterator {
//! # 	type Iter: DistributedIterator<Item = Self::Item>;
//! # 	type Item;
//! # 	fn into_dist_iter(self) -> Self::Iter where Self: Sized;
//! # 	fn dist_iter_mut(&mut self) -> <&mut Self as IntoDistributedIterator>::Iter where for <'a> &'a mut Self: IntoDistributedIterator {
//! # 		<&mut Self as IntoDistributedIterator>::into_dist_iter(self)
//! # 	}
//! # 	fn dist_iter(&self) -> <&Self as IntoDistributedIterator>::Iter where for <'a> &'a Self: IntoDistributedIterator {
//! # 		<&Self as IntoDistributedIterator>::into_dist_iter(self)
//! # 	}
//! # }
//! # trait DistributedIterator {
//! # 	type Item;
//! # 	fn map<B,F>(self, f: F) -> Map<Self,F> where F: FnMut(Self::Item) -> B, Self: Sized {
//! # 		Map(self, f)
//! # 	}
//! # 	fn sum<S>(self) -> S where S: iter::Sum<Self::Item> + iter::Sum<S>, Self: Sized {
//! # 		unimplemented!()
//! # 	}
//! # }
//! # impl<T> IntoDistributedIterator for [T] {
//! # 	type Iter = !;
//! # 	type Item = !;
//! # 	fn into_dist_iter(self) -> Self::Iter where Self: Sized {
//! # 		unreachable!()
//! # 	}
//! # }
//! # impl DistributedIterator for ! {
//! #     type Item = !;
//! # }
//! # impl<'a,T> IntoDistributedIterator for &'a [T] {
//! # 	type Iter = IterRef<'a,T>;
//! # 	type Item = &'a T;
//! # 	fn into_dist_iter(self) -> Self::Iter where Self: Sized {
//! # 		IterRef(self)
//! # 	}
//! # }
//! # struct IterRef<'a,T:'a>(&'a [T]);
//! # impl<'a,T:'a> DistributedIterator for IterRef<'a,T> {
//! # 	type Item = &'a T;
//! # }
//! # struct Map<I,F>(I,F);
//! # impl<I,F> DistributedIterator for Map<I,F> where I: DistributedIterator {
//! # 	type Item = I::Item;
//! # }
//! fn sum_of_squares(input: &[i32]) -> i32 {
//! 	input.dist_iter()
//! 		.map(Fn!(|&i| i * i))
//! 		.sum()
//! }
//! ```
//!
//! For example, if you have multiple forks of a process, or the same binary
//! running on each of a cluster of machines, this library would help you to
//! send closures between them.
//!
//! This library aims to work in as simple and un-magical a way as possible. It
//! currently requires nightly Rust for the `unboxed_closures` and `fn_traits`
//! features (rust issue
//! [#29625](https://github.com/rust-lang/rust/issues/29625)).
//!
//!  * There are three macros, [FnOnce](macro@FnOnce), [FnMut](macro@FnMut) and
//! [Fn](macro@Fn), corresponding to the three types of Rust closure.
//!  * The *captured variables*, i.e. those variables that are referenced by the
//! closure but are declared outside of it, must be explicitly listed.
//!  * There are currently some minor limitations of syntax over normal closure
//! syntax, which are documented below.
//!  * The closure is coerced to a function pointer, which is wrapped by
//! [relative::Code](https://docs.rs/relative) such that it can safely be
//! sent between processes.
//!
//! # Examples of wrapped closures
//! **Inferred, non-capturing closure:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! # (
//! |a| a+1
//! # )(0i32);
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! # (
//! FnMut!(|a| a+1)
//! # )(0i32);
//! ```
//!
//! **Annotated, non-capturing closure:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! # (
//! |a: String| -> String { a.to_uppercase() }
//! # )(String::from("abc"));
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! # (
//! FnMut!(|a: String| -> String { a.to_uppercase() })
//! # )(String::from("abc"));
//! ```
//!
//! **Inferred closure, capturing `num`:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let mut num = 0;
//! # (
//! |a| num += a
//! # )(1i32);
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let mut num = 0;
//! # (
//! FnMut!([num] |a| *num += a)
//! # )(1i32);
//! ```
//! Note: As this is a FnMut closure, `num` is a mutable reference, and must be
//! dereferenced to use.
//!
//! **`move` closure, capturing `hello` and `world`:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let hello = String::from("hello");
//! let mut world = String::new();
//! # (
//! move |name| {
//! 	world += (hello.to_uppercase() + name).as_str();
//! }
//! # )("abc");
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let hello = String::from("hello");
//! let mut world = String::new();
//! # (
//! FnMut!([hello,world] move |name| {
//! 	*world += (hello.to_uppercase() + name).as_str();
//! })
//! # )("abc");
//! ```
//! Note: `world` must be dereferenced to use.
//!
//! # Cosmetic limitations
//! As visible above, there are currently some minor limitations:
//!  * The captured variables in FnMut and Fn closures are references, so need
//! to be dereferenced;
//!  * Compiler errors are not as helpful as normal:
//! ```text
//! error[E0308]: mismatched types
//! ...
//!    = note: expected type `for<..> fn(&'r mut (..), (..))`
//!               found type `[closure@<FnMut macros>:9:9: 10:44 my_var:_]`
//! ```
//! means that `my_var` is a captured variable, but was not explicitly listed.

#![doc(html_root_url = "https://docs.rs/serde_closure/0.1.2")]
#![feature(unboxed_closures, fn_traits, core_intrinsics)]
#![warn(
	missing_copy_implementations,
	missing_debug_implementations,
	missing_docs,
	trivial_numeric_casts,
	unused_extern_crates,
	unused_import_braces,
	unused_qualifications,
	unused_results,
)] // from https://github.com/rust-unofficial/patterns/blob/master/anti_patterns/deny-warnings.md
#![cfg_attr(feature = "cargo-clippy", warn(clippy_pedantic))]
#![cfg_attr(feature = "cargo-clippy", allow(inline_always, doc_markdown))]

extern crate relative;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[cfg(test)]
extern crate bincode;
#[cfg(test)]
extern crate serde_json;

use relative::Code;
use std::{cmp, fmt, hash, intrinsics, marker, mem, ops};

/// A struct representing a serializable closure, created by the
/// [FnOnce](macro@FnOnce) macro. Implements [std::ops::FnOnce], serde's
/// [Serialize](serde::ser::Serialize) and
/// [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// It is generic over `E`: a tuple of the environment variables passed to the
/// [FnOnce](macro@FnOnce) macro; and `F`: the signature of closure as coerced
/// to a function pointer.
///
/// See the [readme](self) for examples.
#[derive(Serialize, Deserialize)]
#[serde(
	bound(serialize = "E: serde::ser::Serialize, F: 'static"),
	bound(deserialize = "E: serde::de::Deserialize<'de>, F: 'static")
)]
pub struct FnOnce<E, F> {
	env: E,
	addr: Code<F>,
	#[serde(skip)]
	marker: marker::PhantomData<F>,
}
impl<E, F> FnOnce<E, F> {
	#[doc(hidden)]
	#[inline(always)]
	/// Fn pointers from coerced closures will, bar an extremely odd turn of
	/// events, point into the same segment as the base used by
	/// [relative::Code], thus upholding [relative::Code::from()]'s unsafe
	/// contract.
	pub unsafe fn private_construct(env: E, addr: *const (), _: &F) -> Self {
		Self {
			env,
			addr: Code::from(addr),
			marker: marker::PhantomData,
		}
	}
}
impl<E, T, F, O> ops::FnOnce<T> for FnOnce<E, F>
where
	F: ops::FnOnce(E, T) -> O,
{
	type Output = O;
	#[inline(always)]
	extern "rust-call" fn call_once(self, args: T) -> Self::Output {
		// This is fine *as long as* private_construct is given the same fn pointer in last 2 args.
		// This is always true for what should be the only caller: the macro.
		// This seems necessary to avoid bounding the lifetimes of the fn arguments, which reduces
		// the utility substantially.
		unsafe { mem::transmute::<*const (), fn(E, T) -> F::Output>(self.addr.to()) }
			.call_once((self.env, args))
	}
}
impl<E, T> Clone for FnOnce<E, T>
where
	E: Clone,
{
	fn clone(&self) -> Self {
		Self {
			env: self.env.clone(),
			addr: self.addr,
			marker: marker::PhantomData,
		}
	}
}
impl<E, T> Copy for FnOnce<E, T> where E: Copy {}
impl<E, T> PartialEq for FnOnce<E, T>
where
	E: PartialEq,
{
	fn eq(&self, other: &Self) -> bool {
		self.env == other.env && self.addr == other.addr
	}
}
impl<E, T> Eq for FnOnce<E, T> where E: Eq {}
impl<E, T> hash::Hash for FnOnce<E, T>
where
	E: hash::Hash,
{
	fn hash<H: hash::Hasher>(&self, state: &mut H) {
		self.env.hash(state);
		self.addr.hash(state);
	}
}
impl<E, T> PartialOrd for FnOnce<E, T>
where
	E: PartialOrd,
{
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.env
			.partial_cmp(&other.env)
			.map(|x| x.then_with(|| self.addr.cmp(&other.addr)))
	}
}
impl<E, T> Ord for FnOnce<E, T>
where
	E: Ord,
{
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.env
			.cmp(&other.env)
			.then_with(|| self.addr.cmp(&other.addr))
	}
}
impl<E, T> fmt::Debug for FnOnce<E, T>
where
	E: fmt::Debug,
{
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		fmt.debug_struct("FnOnce")
			.field("env", &self.env)
			.field(unsafe { intrinsics::type_name::<T>() }, &self.addr)
			.finish()
	}
}

/// A struct representing a serializable closure, created by the
/// [FnMut](macro@FnMut) macro. Implements [std::ops::FnMut], serde's
/// [Serialize](serde::ser::Serialize) and
/// [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// It is generic over `E`: a tuple of the environment variables passed to the
/// [FnMut](macro@FnMut) macro; and `F`: the signature of closure as coerced to
/// a function pointer.
///
/// See the [readme](self) for examples.
#[derive(Serialize, Deserialize)]
#[serde(
	bound(serialize = "E: serde::ser::Serialize, F: 'static"),
	bound(deserialize = "E: serde::de::Deserialize<'de>, F: 'static")
)]
pub struct FnMut<E, F> {
	env: E,
	addr: Code<F>,
	#[serde(skip)]
	marker: marker::PhantomData<F>,
}
impl<E, F> FnMut<E, F> {
	#[doc(hidden)]
	#[inline(always)]
	/// Fn pointers from coerced closures will, bar an extremely odd turn of
	/// events, point into the same segment as the base used by
	/// [relative::Code], thus upholding [relative::Code::from()]'s unsafe
	/// contract.
	pub unsafe fn private_construct(env: E, addr: *const (), _: &F) -> Self {
		Self {
			env,
			addr: Code::from(addr),
			marker: marker::PhantomData,
		}
	}
}
impl<E, T, F, O> ops::FnOnce<T> for FnMut<E, F>
where
	F: ops::FnOnce(&mut E, T) -> O,
{
	type Output = O;
	#[inline(always)]
	extern "rust-call" fn call_once(mut self, args: T) -> Self::Output {
		// This is fine *as long as* private_construct is given the same fn pointer in last 2 args.
		// This is always true for what should be the only caller: the macro.
		// This seems necessary to avoid bounding the lifetimes of the fn arguments, which reduces
		// the utility substantially.
		unsafe { mem::transmute::<*const (), fn(&mut E, T) -> F::Output>(self.addr.to()) }
			.call_once((&mut self.env, args))
	}
}
impl<E, T, F, O> ops::FnMut<T> for FnMut<E, F>
where
	F: ops::FnOnce(&mut E, T) -> O,
{
	#[inline(always)]
	extern "rust-call" fn call_mut(&mut self, args: T) -> Self::Output {
		// This is fine *as long as* private_construct is given the same fn pointer in last 2 args.
		// This is always true for what should be the only caller: the macro.
		// This seems necessary to avoid bounding the lifetimes of the fn arguments, which reduces
		// the utility substantially.
		unsafe { mem::transmute::<*const (), fn(&mut E, T) -> F::Output>(self.addr.to()) }
			.call_mut((&mut self.env, args))
	}
}
impl<E, T> Clone for FnMut<E, T>
where
	E: Clone,
{
	fn clone(&self) -> Self {
		Self {
			env: self.env.clone(),
			addr: self.addr,
			marker: marker::PhantomData,
		}
	}
}
impl<E, T> Copy for FnMut<E, T> where E: Copy {}
impl<E, T> PartialEq for FnMut<E, T>
where
	E: PartialEq,
{
	fn eq(&self, other: &Self) -> bool {
		self.env == other.env && self.addr == other.addr
	}
}
impl<E, T> Eq for FnMut<E, T> where E: Eq {}
impl<E, T> hash::Hash for FnMut<E, T>
where
	E: hash::Hash,
{
	fn hash<H: hash::Hasher>(&self, state: &mut H) {
		self.env.hash(state);
		self.addr.hash(state);
	}
}
impl<E, T> PartialOrd for FnMut<E, T>
where
	E: PartialOrd,
{
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.env
			.partial_cmp(&other.env)
			.map(|x| x.then_with(|| self.addr.cmp(&other.addr)))
	}
}
impl<E, T> Ord for FnMut<E, T>
where
	E: Ord,
{
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.env
			.cmp(&other.env)
			.then_with(|| self.addr.cmp(&other.addr))
	}
}
impl<E, T> fmt::Debug for FnMut<E, T>
where
	E: fmt::Debug,
{
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		fmt.debug_struct("FnMut")
			.field("env", &self.env)
			.field(unsafe { intrinsics::type_name::<T>() }, &self.addr)
			.finish()
	}
}

/// A struct representing a serializable closure, created by the [Fn](macro@Fn)
/// macro. Implements [std::ops::Fn], serde's [Serialize](serde::ser::Serialize)
/// and [Deserialize](serde::de::DeserializeOwned), and various convenience
/// traits.
///
/// It is generic over `E`: a tuple of the environment variables passed to the
/// [Fn](macro@Fn) macro; and `F`: the signature of closure as coerced to a
/// function pointer.
///
/// See the [readme](self) for examples.
#[derive(Serialize, Deserialize)]
#[serde(
	bound(serialize = "E: serde::ser::Serialize, F: 'static"),
	bound(deserialize = "E: serde::de::Deserialize<'de>, F: 'static")
)]
pub struct Fn<E, F> {
	env: E,
	addr: Code<F>,
	#[serde(skip)]
	marker: marker::PhantomData<F>,
}
impl<E, F> Fn<E, F> {
	#[doc(hidden)]
	#[inline(always)]
	/// Fn pointers from coerced closures will, bar an extremely odd turn of
	/// events, point into the same segment as the base used by
	/// [relative::Code], thus upholding [relative::Code::from()]'s unsafe
	/// contract.
	pub unsafe fn private_construct(env: E, addr: *const (), _: &F) -> Self {
		Self {
			env,
			addr: Code::from(addr),
			marker: marker::PhantomData,
		}
	}
}
impl<E, T, F, O> ops::FnOnce<T> for Fn<E, F>
where
	F: ops::FnOnce(&E, T) -> O,
{
	type Output = O;
	#[inline(always)]
	extern "rust-call" fn call_once(self, args: T) -> Self::Output {
		// This is fine *as long as* private_construct is given the same fn pointer in last 2 args.
		// This is always true for what should be the only caller: the macro.
		// This seems necessary to avoid bounding the lifetimes of the fn arguments, which reduces
		// the utility substantially.
		unsafe { mem::transmute::<*const (), fn(&E, T) -> F::Output>(self.addr.to()) }
			.call_once((&self.env, args))
	}
}
impl<E, T, F, O> ops::FnMut<T> for Fn<E, F>
where
	F: ops::FnOnce(&E, T) -> O,
{
	#[inline(always)]
	extern "rust-call" fn call_mut(&mut self, args: T) -> Self::Output {
		// This is fine *as long as* private_construct is given the same fn pointer in last 2 args.
		// This is always true for what should be the only caller: the macro.
		// This seems necessary to avoid bounding the lifetimes of the fn arguments, which reduces
		// the utility substantially.
		unsafe { mem::transmute::<*const (), fn(&E, T) -> F::Output>(self.addr.to()) }
			.call_mut((&self.env, args))
	}
}
impl<E, T, F, O> ops::Fn<T> for Fn<E, F>
where
	F: ops::FnOnce(&E, T) -> O,
{
	#[inline(always)]
	extern "rust-call" fn call(&self, args: T) -> Self::Output {
		// This is fine *as long as* private_construct is given the same fn pointer in last 2 args.
		// This is always true for what should be the only caller: the macro.
		// This seems necessary to avoid bounding the lifetimes of the fn arguments, which reduces
		// the utility substantially.
		unsafe { mem::transmute::<*const (), fn(&E, T) -> F::Output>(self.addr.to()) }
			.call((&self.env, args))
	}
}
impl<E, T> Clone for Fn<E, T>
where
	E: Clone,
{
	fn clone(&self) -> Self {
		Self {
			env: self.env.clone(),
			addr: self.addr,
			marker: marker::PhantomData,
		}
	}
}
impl<E, T> Copy for Fn<E, T> where E: Copy {}
impl<E, T> PartialEq for Fn<E, T>
where
	E: PartialEq,
{
	fn eq(&self, other: &Self) -> bool {
		self.env == other.env && self.addr == other.addr
	}
}
impl<E, T> Eq for Fn<E, T> where E: Eq {}
impl<E, T> hash::Hash for Fn<E, T>
where
	E: hash::Hash,
{
	fn hash<H: hash::Hasher>(&self, state: &mut H) {
		self.env.hash(state);
		self.addr.hash(state);
	}
}
impl<E, T> PartialOrd for Fn<E, T>
where
	E: PartialOrd,
{
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.env
			.partial_cmp(&other.env)
			.map(|x| x.then_with(|| self.addr.cmp(&other.addr)))
	}
}
impl<E, T> Ord for Fn<E, T>
where
	E: Ord,
{
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.env
			.cmp(&other.env)
			.then_with(|| self.addr.cmp(&other.addr))
	}
}
impl<E, T> fmt::Debug for Fn<E, T>
where
	E: fmt::Debug,
{
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		fmt.debug_struct("Fn")
			.field("env", &self.env)
			.field(unsafe { intrinsics::type_name::<T>() }, &self.addr)
			.finish()
	}
}

/// Macro that wraps a closure, evaluating to a [FnOnce](struct@FnOnce) struct
/// that implements [std::ops::FnOnce], serde's
/// [Serialize](serde::ser::Serialize) and
/// [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// See the [readme](self) for examples.
#[macro_export]
macro_rules! FnOnce {
	([$( $env:ident ,)* ] move |$( $arg:pat => $ty:ty, $t:ident $u:ty,)*| -> $o:ty $block:block) => ({
		let env = ($($env,)*);
		fn apply_env_type<E,O,$($t,)*>(_ :&E, f: fn(E,($($u,)*))->O) -> fn(E,($($u,)*))->O {f}
		let closure = apply_env_type(&env, move|($($env,)*),($($arg,)*):($($ty,)*)|->$o { $block });
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(env,unreachable!());
		}
		let fn_ptr = closure as fn(_,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(env,unreachable!());
		}
		#[allow(unused_unsafe)]
		unsafe {
			$crate::FnOnce::private_construct(
				env,
				fn_ptr as *const (),
				&fn_ptr,
			)
		}
	});
	([$( $env:ident ,)* ] ref |$( $arg:pat => $ty:ty, $t:ident $u:ty,)*| -> $o:ty $block:block) => ({
		let env = ($(&$env,)*);
		fn apply_env_type<E,O,$($t,)*>(_ :&E, f: fn(E,($($u,)*))->O) -> fn(E,($($u,)*))->O {f}
		let closure = apply_env_type(&env, |($($env,)*),($($arg,)*):($($ty,)*)|->$o { $block });
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(env,unreachable!());
		}
		let fn_ptr = closure as fn(_,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(env,unreachable!());
		}
		#[allow(unused_unsafe)]
		unsafe {
			$crate::FnOnce::private_construct(
				env,
				fn_ptr as *const (),
				&fn_ptr,
			)
		}
	});

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:pat, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => &mut _, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:pat, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => &_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => _, $s $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:ident: $type_:ty, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => $type_, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:ident: $type_:ty, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => $type_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: &mut $type_:ty, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => &mut $type_, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: & $type_:ty, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => & $type_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: $type_:ty, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat=> $type_:ty, $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) | $( $tail )*) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:pat $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => &mut _, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:pat $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => &_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => _, $s $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => $type_, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => $type_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: &mut $type_:ty $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => &mut $type_, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: & $type_:ty $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => & $type_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat=> $type_:ty $(,)*| $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) || $( $tail )*) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($($ss:ident)*) || $block:expr) => { FnOnce!([$($env,)*] $move |$($arg => $type,$t $u,)*| -> _ { $block } ) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($($ss:ident)*) || -> $o:ty $block:block) => { FnOnce!([$($env,)*] $move |$($arg => $type,$t $u,)*| -> $o $block ) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| () |$( $tail:tt )*) => { compile_error!("This macro unfortunately only handles up to 32 arguments. Easily extendable, fork and bump it if you really need that many!") };

	([$( $env:ident),* $(,)* ] move | $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	(move | $( $tail:tt )*) => { FnOnce!(@args [] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	([$( $env:ident),* $(,)* ] move || $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	(move || $( $tail:tt )*) => { FnOnce!(@args [] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	([$( $env:ident),* $(,)* ] | $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	(| $( $tail:tt )*) => { FnOnce!(@args [] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	([$( $env:ident),* $(,)* ] || $( $tail:tt )*) => { FnOnce!(@args [$($env,)*] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	(|| $( $tail:tt )*) => { FnOnce!(@args [] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
}

/// Macro that wraps a closure, evaluating to a [FnMut](struct@FnMut) struct
/// that implements [std::ops::FnMut], serde's
/// [Serialize](serde::ser::Serialize) and
/// [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// See the [readme](self) for examples.
#[macro_export]
macro_rules! FnMut {
	([$( $env:ident ,)* ] move |$( $arg:pat => $ty:ty, $t:ident $u:ty,)*| -> $o:ty $block:block) => ({
		let mut env = ($($env,)*);
		fn apply_env_type<E,O,$($t,)*>(_ :&E, f: fn(&mut E,($($u,)*))->O) -> fn(&mut E,($($u,)*))->O {f}
		let closure = apply_env_type(&env, move|&mut ($(ref mut $env,)*):&mut _,($($arg,)*):($($ty,)*)|->$o { $block });
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(&mut env,unreachable!());
		}
		let fn_ptr = closure as fn(&mut _,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(&mut env,unreachable!());
		}
		#[allow(unused_unsafe)]
		unsafe {
			$crate::FnMut::private_construct(
				env,
				fn_ptr as *const (),
				&fn_ptr,
			)
		}
	});
	([$( $env:ident ,)* ] ref |$( $arg:pat => $ty:ty, $t:ident $u:ty,)*| -> $o:ty $block:block) => ({
		let mut env = ($(&mut $env,)*);
		fn apply_env_type<E,O,$($t,)*>(_ :&E, f: fn(&mut E,($($u,)*))->O) -> fn(&mut E,($($u,)*))->O {f}
		let closure = apply_env_type(&env, |&mut ($(&mut ref mut $env,)*):&mut _,($($arg,)*):($($ty,)*)|->$o { $block });
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(&mut env,unreachable!());
		}
		let fn_ptr = closure as fn(&mut _,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(&mut env,unreachable!());
		}
		#[allow(unused_unsafe)]
		unsafe {
			$crate::FnMut::private_construct(
				env,
				fn_ptr as *const (),
				&fn_ptr,
			)
		}
	});

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:pat, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => &mut _, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:pat, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => &_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => _, $s $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:ident: $type_:ty, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => $type_, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:ident: $type_:ty, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => $type_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: &mut $type_:ty, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => &mut $type_, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: & $type_:ty, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => & $type_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: $type_:ty, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat=> $type_:ty, $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) | $( $tail )*) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:pat $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => &mut _, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:pat $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => &_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => _, $s $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => $type_, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => $type_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: &mut $type_:ty $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => &mut $type_, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: & $type_:ty $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => & $type_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat=> $type_:ty $(,)*| $( $tail:tt )*) => { FnMut!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) || $( $tail )*) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($($ss:ident)*) || $block:expr) => { FnMut!([$($env,)*] $move |$($arg => $type,$t $u,)*| -> _ { $block } ) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($($ss:ident)*) || -> $o:ty $block:block) => { FnMut!([$($env,)*] $move |$($arg => $type,$t $u,)*| -> $o $block ) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| () |$( $tail:tt )*) => { compile_error!("This macro unfortunately only handles up to 32 arguments. Easily extendable, fork and bump it if you really need that many!") };

	([$( $env:ident),* $(,)* ] move | $( $tail:tt )*) => { FnMut!(@args [$($env,)*] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	(move | $( $tail:tt )*) => { FnMut!(@args [] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	([$( $env:ident),* $(,)* ] move || $( $tail:tt )*) => { FnMut!(@args [$($env,)*] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	(move || $( $tail:tt )*) => { FnMut!(@args [] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	([$( $env:ident),* $(,)* ] | $( $tail:tt )*) => { FnMut!(@args [$($env,)*] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	(| $( $tail:tt )*) => { FnMut!(@args [] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	([$( $env:ident),* $(,)* ] || $( $tail:tt )*) => { FnMut!(@args [$($env,)*] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	(|| $( $tail:tt )*) => { FnMut!(@args [] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
}

/// Macro that wraps a closure, evaluating to a [Fn](struct@Fn) struct that
/// implements [std::ops::Fn], serde's [Serialize](serde::ser::Serialize) and
/// [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// See the [readme](self) for examples.
#[macro_export]
macro_rules! Fn {
	([$( $env:ident ,)* ] move |$( $arg:pat => $ty:ty, $t:ident $u:ty,)*| -> $o:ty $block:block) => ({
		let env = ($($env,)*);
		fn apply_env_type<E,O,$($t,)*>(_ :&E, f: fn(&E,($($u,)*))->O) -> fn(&E,($($u,)*))->O {f}
		let closure = apply_env_type(&env, move|&($(ref $env,)*):&_,($($arg,)*):($($ty,)*)|->$o { $block });
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(&env,unreachable!());
		}
		let fn_ptr = closure as fn(&_,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(&env,unreachable!());
		}
		#[allow(unused_unsafe)]
		unsafe {
			$crate::Fn::private_construct(
				env,
				fn_ptr as *const (),
				&fn_ptr,
			)
		}
	});
	([$( $env:ident ,)* ] ref |$( $arg:pat => $ty:ty, $t:ident $u:ty,)*| -> $o:ty $block:block) => ({
		let env = ($(&$env,)*);
		fn apply_env_type<E,O,$($t,)*>(_ :&E, f: fn(&E,($($u,)*))->O) -> fn(&E,($($u,)*))->O {f}
		let closure = apply_env_type(&env, |&($($env,)*):&_,($($arg,)*):($($ty,)*)|->$o { $block });
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(&env,unreachable!());
		}
		let fn_ptr = closure as fn(&_,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(&env,unreachable!());
		}
		#[allow(unused_unsafe)]
		unsafe {
			$crate::Fn::private_construct(
				env,
				fn_ptr as *const (),
				&fn_ptr,
			)
		}
	});

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:pat, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => &mut _, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:pat, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => &_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => _, $s $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:ident: $type_:ty, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => $type_, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:ident: $type_:ty, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => $type_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: &mut $type_:ty, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => &mut $type_, $s &mut $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: & $type_:ty, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => & $type_, $s & $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: $type_:ty, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) | $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat=> $type_:ty, $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) | $( $tail )*) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:pat $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => &mut _, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:pat $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => &_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => _, $s $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |&mut $arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* &mut $arg_ => $type_, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |& $arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* & $arg_ => $type_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: &mut $type_:ty $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => &mut $type_, $s &mut $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: & $type_:ty $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => & $type_, $s & $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:ident: $type_:ty $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) || $( $tail )*) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($s:ident $($ss:ident)*) |$arg_:pat=> $type_:ty $(,)*| $( $tail:tt )*) => { Fn!(@args [$($env,)*] $move |$($arg => $type,$t $u,)* $arg_ => $type_, $s $s,| ($($ss)*) || $( $tail )*) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($($ss:ident)*) || $block:expr) => { Fn!([$($env,)*] $move |$($arg => $type,$t $u,)*| -> _ { $block } ) };
	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| ($($ss:ident)*) || -> $o:ty $block:block) => { Fn!([$($env,)*] $move |$($arg => $type,$t $u,)*| -> $o $block ) };

	(@args [$( $env:ident ,)* ] $move:ident |$( $arg:pat => $type:ty, $t:ident $u:ty,)*| () |$( $tail:tt )*) => { compile_error!("This macro unfortunately only handles up to 32 arguments. Easily extendable, fork and bump it if you really need that many!") };

	([$( $env:ident),* $(,)* ] move | $( $tail:tt )*) => { Fn!(@args [$($env,)*] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	(move | $( $tail:tt )*) => { Fn!(@args [] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	([$( $env:ident),* $(,)* ] move || $( $tail:tt )*) => { Fn!(@args [$($env,)*] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	(move || $( $tail:tt )*) => { Fn!(@args [] move | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	([$( $env:ident),* $(,)* ] | $( $tail:tt )*) => { Fn!(@args [$($env,)*] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	(| $( $tail:tt )*) => { Fn!(@args [] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) | $( $tail )*) };
	([$( $env:ident),* $(,)* ] || $( $tail:tt )*) => { Fn!(@args [$($env,)*] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
	(|| $( $tail:tt )*) => { Fn!(@args [] ref | | (T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 T31 T32) || $( $tail )*) };
}

#[cfg(test)]
mod tests {
	#![cfg_attr(
		feature = "cargo-clippy",
		allow(items_after_statements, type_complexity)
	)]
	use bincode;
	use serde;
	use serde_json;
	use std::{env, fmt, mem, process, str};
	#[test]
	fn fn_ptr_size() {
		assert_eq!(mem::size_of::<usize>(), mem::size_of::<fn()>());
	}
	#[test]
	fn fnonce() {
		fn test<
			F: FnOnce(usize, &usize, &mut usize, String, &String, &mut String) -> String
				+ serde::ser::Serialize
				+ serde::de::DeserializeOwned
				+ PartialEq
				+ Eq
				+ Clone
				+ fmt::Debug,
		>(
			f: F,
		) {
			let deserialized: F =
				serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
			let deserialized2: F = bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
			assert_eq!(f, deserialized);
			assert_eq!(deserialized, deserialized2);
			assert_eq!(f, deserialized2);
			let test = |f: F| {
				let mut a = 3;
				let mut b = String::from("ghi");
				assert_eq!(
					f.clone()(
						1,
						&2,
						&mut a,
						String::from("abc"),
						&String::from("def"),
						&mut b
					),
					"3qwerty129abcdefghiqwertyabcdef"
				);
				a += 6;
				b += "pqr";
				assert_eq!(
					f.clone()(
						4,
						&5,
						&mut a,
						String::from("jkl"),
						&String::from("mno"),
						&mut b
					),
					"3qwerty4527jklmnoghiqwertyabcdefpqrqwertyjklmno"
				);
				a += 9;
				b += "yz";
				assert_eq!(
					f(
						7,
						&8,
						&mut a,
						String::from("stu"),
						&String::from("vwx"),
						&mut b
					),
					"3qwerty7854stuvwxghiqwertyabcdefpqrqwertyjklmnoyzqwertystuvwx"
				);
			};
			test(f);
			test(deserialized);
			test(deserialized2);
		}
		let (a, b) = (3_usize, String::from("qwerty"));
		let a = FnOnce!([a,b] move |c,d:&_,e:&mut _,f:String,g:&String,h:&mut String| -> String {
			let a: usize = a;
			let b: String = b;
			*e += a+c+*d;
			// *a += *e;
			*h += (b.clone()+f.as_str()+g.as_str()).as_str();
			// *b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		});
		test(a);
		let b = FnOnce!(|a| {
			println!("{:?}", a);
		});
		b(0_usize);
		let c = FnOnce!(|arg: String| {
			println!("{}", arg);
		});
		let _ = (c, c);
	}
	#[test]
	fn fnmut() {
		fn test<
			F: FnMut(usize, &usize, &mut usize, String, &String, &mut String) -> String
				+ serde::ser::Serialize
				+ serde::de::DeserializeOwned
				+ PartialEq
				+ Eq
				+ Clone
				+ fmt::Debug,
		>(
			mut f: F,
		) {
			let mut deserialized: F =
				serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
			let mut deserialized2: F =
				bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
			assert_eq!(f, deserialized);
			assert_eq!(deserialized, deserialized2);
			assert_eq!(f, deserialized2);
			let test = |f: &mut F| {
				let mut a = 3;
				let mut b = String::from("ghi");
				assert_eq!(
					f(
						1,
						&2,
						&mut a,
						String::from("abc"),
						&String::from("def"),
						&mut b
					),
					"12qwertyghiqwertyabcdef129abcdefghiqwertyabcdef"
				);
				a += 6;
				b += "pqr";
				assert_eq!(f(4, &5, &mut a, String::from("jkl"), &String::from("mno"), &mut b), "48qwertyghiqwertyabcdefghiqwertyabcdefpqrqwertyghiqwertyabcdefjklmno4536jklmnoghiqwertyabcdefpqrqwertyghiqwertyabcdefjklmno");
				a += 9;
				b += "yz";
				assert_eq!(f(7, &8, &mut a, String::from("stu"), &String::from("vwx"), &mut b), "156qwertyghiqwertyabcdefghiqwertyabcdefpqrqwertyghiqwertyabcdefjklmnoghiqwertyabcdefpqrqwertyghiqwertyabcdefjklmnoyzqwertyghiqwertyabcdefghiqwertyabcdefpqrqwertyghiqwertyabcdefjklmnostuvwx78108stuvwxghiqwertyabcdefpqrqwertyghiqwertyabcdefjklmnoyzqwertyghiqwertyabcdefghiqwertyabcdefpqrqwertyghiqwertyabcdefjklmnostuvwx");
			};
			test(&mut f);
			test(&mut deserialized);
			test(&mut deserialized2);
			assert_eq!(f, deserialized);
			assert_eq!(deserialized, deserialized2);
			assert_eq!(f, deserialized2);
		}
		let (a, b) = (3_usize, String::from("qwerty"));
		let a = FnMut!([a,b] move |c,d:&_,e:&mut _,f:String,g:&String,h:&mut String| -> String {
			*e += *a+c+*d;
			*a += *e;
			*h += (b.clone()+f.as_str()+g.as_str()).as_str();
			*b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		});
		test(a);
		fn unfold<A, St, F>(initial_state: St, f: F) -> Unfold<St, F>
		where
			F: FnMut(&mut St) -> Option<A> + serde::ser::Serialize,
		{
			Unfold {
				_f: f,
				_state: initial_state,
			}
		}
		struct Unfold<St, F> {
			_f: F,
			_state: St,
		}
		let _ = unfold(0_usize, FnMut!(|acc: &mut _| Some(*acc)));
		let x = 123_usize;
		let c = FnMut!([x] move|arg:String|{
			println!("{} {}", x, arg);
		});
		let _ = (c, c);
	}
	#[test]
	fn fnref() {
		fn test<
			F: Fn(usize, &usize, &mut usize, &usize, &mut usize, String, &String, &mut String)
					-> String
				+ serde::ser::Serialize
				+ serde::de::DeserializeOwned
				+ PartialEq
				+ Eq
				+ Clone
				+ fmt::Debug,
		>(
			mut f: F,
		) {
			let mut deserialized: F =
				serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
			let mut deserialized2: F =
				bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
			assert_eq!(f, deserialized);
			assert_eq!(deserialized, deserialized2);
			assert_eq!(f, deserialized2);
			let test = |f: &mut F| {
				let mut a = 3;
				let mut b = String::from("ghi");
				let mut x = 11;
				assert_eq!(
					f(
						1,
						&2,
						&mut a,
						&10,
						&mut x,
						String::from("abc"),
						&String::from("def"),
						&mut b
					),
					"3qwerty129abcdefghiqwertyabcdef"
				);
				a += 6;
				b += "pqr";
				x += 13;
				assert_eq!(
					f(
						4,
						&5,
						&mut a,
						&12,
						&mut x,
						String::from("jkl"),
						&String::from("mno"),
						&mut b
					),
					"3qwerty4527jklmnoghiqwertyabcdefpqrqwertyjklmno"
				);
				a += 9;
				b += "yz";
				x += 15;
				assert_eq!(
					f(
						7,
						&8,
						&mut a,
						&14,
						&mut x,
						String::from("stu"),
						&String::from("vwx"),
						&mut b
					),
					"3qwerty7854stuvwxghiqwertyabcdefpqrqwertyjklmnoyzqwertystuvwx"
				);
			};
			test(&mut f);
			test(&mut deserialized);
			test(&mut deserialized2);
			assert_eq!(f, deserialized);
			assert_eq!(deserialized, deserialized2);
			assert_eq!(f, deserialized2);
		}
		let (a, b) = (3_usize, String::from("qwerty"));
		{
			assert_eq!(
				Fn!([a,b] |c:usize,d:&usize,e:&mut usize,&_x:&usize,&mut _y:&mut usize,f:String,g:&String,h:&mut String| -> String {
				*e += *a+c+*d;
				// *a += *e;
				*h += (b.clone()+f.as_str()+g.as_str()).as_str();
				// *b += h.as_str();
				format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
			})(
					0,
					&1,
					&mut 2,
					&3,
					&mut 4,
					String::from("a"),
					&String::from("b"),
					&mut String::from("c")
				),
				"3qwerty016abcqwertyab"
			);
		}
		let x = Fn!([a,b] move |c,d:&_,e:&mut _,&_x,&mut _y,f:String,g:&String,h:&mut String| -> String {
			*e += *a+c+*d;
			// *a += *e;
			*h += (b.clone()+f.as_str()+g.as_str()).as_str();
			// *b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		});
		test(x);
		fn unfold<A, St, F>(initial_state: St, f: F) -> Unfold<St, F>
		where
			F: Fn(&mut St) -> Option<A> + serde::ser::Serialize,
		{
			Unfold {
				_f: f,
				_state: initial_state,
			}
		}
		struct Unfold<St, F> {
			_f: F,
			_state: St,
		}
		let _ = unfold(0_usize, Fn!(|acc: &mut _| Some(*acc)));
		let x = 123_usize;
		let c = unsafe {
			// to check unused_unsafe in macro
			Fn!([x] move|arg:String|{
			println!("{} {}", x, arg);
		})
		};
		let _ = (c, c);
	}
	#[test]
	fn multi_process() {
		let exe = env::current_exe().unwrap();
		if let Ok(x) = env::var("SPAWNED_TOKEN_SERDECLOSURE") {
			let mut f: super::FnMut<
				(usize, String),
				for<'r, 's, 't0, 't1, 't2> fn(
					&'r mut (usize, String),
					(
						usize,
						&'s usize,
						&'t0 mut usize,
						String,
						&'t1 String,
						&'t2 mut String,
					),
				) -> String,
			> = serde_json::from_str(&x).unwrap();
			let mut a = 3;
			let mut b = String::from("ghi");
			assert_eq!(
				f(
					1,
					&2,
					&mut a,
					String::from("abc"),
					&String::from("def"),
					&mut b
				),
				"12qwertyghiqwertyabcdef129abcdefghiqwertyabcdef"
			);
			println!("success_token_serdeclosure {:?}", f);
			return;
		}
		let (a, b) = (3_usize, String::from("qwerty"));
		let a: super::FnMut<
			(usize, String),
			for<'r, 's, 't0, 't1, 't2> fn(
				&'r mut (usize, String),
				(
					usize,
					&'s usize,
					&'t0 mut usize,
					String,
					&'t1 String,
					&'t2 mut String,
				),
			) -> String,
		> = FnMut!([a,b] move |c,d:&_,e:&mut _,f:String,g:&String,h:&mut String| -> String {
			*e += *a+c+*d;
			*a += *e;
			*h += (b.clone()+f.as_str()+g.as_str()).as_str();
			*b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		});
		for i in 0..100 {
			let output = process::Command::new(&exe)
				.arg("--nocapture")
				.arg("--exact")
				.arg("tests::multi_process")
				.env(
					"SPAWNED_TOKEN_SERDECLOSURE",
					serde_json::to_string(&a).unwrap(),
				).output()
				.unwrap();
			if !str::from_utf8(&output.stdout)
				.unwrap()
				.contains("success_token_serdeclosure")
				|| !output.status.success()
			{
				panic!("{}: {:?}", i, output);
			}
		}
	}
}
