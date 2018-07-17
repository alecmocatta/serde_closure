//! Serialisable closures.
//!
//! This library provides macros to wrap closures such that they can serialised and sent between other processes running the same binary.
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
//! #     type Item = I::Item;
//! # }
//! fn sum_of_squares(input: &[i32]) -> i32 {
//! 	input.dist_iter()
//! 		.map(Fn!(|i:&_| *i * *i))
//! 		.sum()
//! }
//! ```
//!
//! For example, if you have the same binary running on each of a cluster of machines, this library would help you to send closures between them.
//!
//! This library aims to work in as simple and un-magical a way as possible. It currently requires nightly Rust for the `unboxed_closures` and `fn_traits` features (rust issue [#29625](https://github.com/rust-lang/rust/issues/29625)).
//!
//!  * There are three macros, [FnOnce](macro@FnOnce), [FnMut](macro@FnMut) and [Fn](macro@Fn), corresponding to the three types of Rust closure.
//!  * The *captured variables*, i.e. those variables that are referenced by the closure but are declared outside of it, must be explicitly listed.
//!  * The closure is coerced to a function pointer, which is serialized as an [isize] relative to a known base address.
//!  * It is deserialised by adding this isize to the known base address, and transmuting to a function pointer.
//!  * This is the only necessitation of unsafety, and is reliant upon the function pointer being positioned identically relative to the base in both processes – hence both binaries must be identical.
//!  * To the best of my knowledge this holds in Rust for a given binary. If somehow the known base and the function pointer are in different objects and are loaded at different relative addresses, then this will fail, very likely as a segfault.
//!  * A solution in this case would be to compile a statically linked executable – in Rust this currently means adding `--target x86_64-unknown-linux-musl` or similar to the cargo or rustc command line.
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
//! **Inferred closure, capturing `num`:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let mut num = 0;
//! # (
//! move |a| num += a
//! # )(1i32);
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let mut num = 0;
//! # (
//! FnMut!([num] move |a| *num += a)
//! # )(1i32);
//! ```
//! Note: If any variables are captured then the `move` keyword must be present. As this is a FnMut closure, `num` is a mutable reference, and must be dereferenced to use.
//!
//! **Capturing `hello` requiring extra annotation:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let mut hello = String::new();
//! # (
//! move |a| {
//! 	hello = hello.to_uppercase() + a;
//! 	hello.clone()
//! }
//! # )("abc");
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let mut hello = String::new();
//! # (
//! FnMut!([hello] move |a| {
//! 	let hello: &mut String = hello;
//! 	*hello = hello.to_uppercase() + a;
//! 	hello.clone()
//! })
//! # )("abc");
//! ```
//! Note: `hello` needs its type annotated in the closure.
//!
//! **Complex closure, capturing `a` and `b`:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let (mut a, mut b) = (1usize, String::from("foo"));
//! # (
//! move |c,d:&_,e: &mut _,f:String,g:&String,h:&mut String| {
//! 	*e += a+c+*d;
//! 	a += *e;
//! 	*h += ((b.clone()+f.as_str()+g.as_str())).as_str();
//! 	b += h.as_str();
//! }
//! # )(1usize, &2usize, &mut 3usize, String::from("abc"), &String::from("def"), &mut String::from("ghi"));
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let (mut a, mut b) = (1usize, String::from("foo"));
//! # (
//! FnMut!([a,b] move |c:_,d:&_,e:&mut _,f:String,g:&String,h:&mut String| {
//! 	let b: &mut String = b;
//! 	*e += *a+c+*d;
//! 	*a += *e;
//! 	*h += ((b.clone()+f.as_str()+g.as_str())).as_str();
//! 	*b += h.as_str();
//! })
//! # )(1usize, &2usize, &mut 3usize, String::from("abc"), &String::from("def"), &mut String::from("ghi"));
//! ```
//!
//! # Cosmetic limitations
//! As visible above, there are currently some limitations that often necessitate extra annotation that you might typically expect to be redundant.
//!  * Type inference doesn't work as well as normal, hence extra type annotations might be needed;
//!  * The captured variables in FnMut and FnRef closures are references, so need to be dereferenced;
//!  * Types cannot be annotated in the list of captured variables;
//!  * Either none or all of the closure arguments must be annotated; though `_` can be used;
//!  * The `move` keyword must be present if any variables are captured.

#![doc(html_root_url = "https://docs.rs/serde_closure/0.1.0")]
#![feature(
	unboxed_closures,
	fn_traits,
	core_intrinsics,
	macro_at_most_once_rep,
	allow_internal_unstable
)]
#![deny(missing_docs, warnings, deprecated)]

extern crate serde;
#[macro_use]
extern crate serde_derive;
#[cfg(test)]
extern crate bincode;
#[cfg(test)]
extern crate serde_json;

use std::{cmp, fmt, hash, intrinsics, marker, mem, ops};

/// A struct representing a serialisable closure, created by the [FnOnce](macro@FnOnce) macro. Implements [std::ops::FnOnce], serde's [Serialize](serde::ser::Serialize) and [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// It is generic over `E`: a tuple of the environment variables passed to the [FnOnce](macro@FnOnce) macro; and `F`: the signature of closure as coerced to a function pointer.
///
/// See the [readme](self) for examples.
#[derive(Serialize, Deserialize)]
#[serde(
	bound(serialize = "E: serde::ser::Serialize"),
	bound(deserialize = "E: serde::de::Deserialize<'de>")
)]
pub struct FnOnce<E, F> {
	env: E,
	addr: usize,
	#[serde(skip)]
	marker: marker::PhantomData<F>,
}
impl<E, T> FnOnce<E, T> {
	#[doc(hidden)]
	#[inline(always)]
	pub fn private_construct(env: E, addr: usize, _: &T) -> FnOnce<E, T> {
		FnOnce {
			env,
			addr,
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
		unsafe { mem::transmute::<usize, fn(E, T) -> F::Output>(self.addr) }
			.call_once((self.env, args))
	}
}
impl<E, T> Clone for FnOnce<E, T>
where
	E: Clone,
{
	fn clone(&self) -> Self {
		FnOnce {
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
impl<E, T> cmp::PartialOrd for FnOnce<E, T>
where
	E: cmp::PartialOrd,
{
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.env
			.partial_cmp(&other.env)
			.map(|x| x.then_with(|| self.addr.cmp(&other.addr)))
	}
}
impl<E, T> cmp::Ord for FnOnce<E, T>
where
	E: cmp::Ord,
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
			.field(
				unsafe { intrinsics::type_name::<T>() },
				&(self.addr as *const ()),
			)
			.finish()
	}
}

/// A struct representing a serialisable closure, created by the [FnMut](macro@FnMut) macro. Implements [std::ops::FnMut], serde's [Serialize](serde::ser::Serialize) and [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// It is generic over `E`: a tuple of the environment variables passed to the [FnMut](macro@FnMut) macro; and `F`: the signature of closure as coerced to a function pointer.
///
/// See the [readme](self) for examples.
#[derive(Serialize, Deserialize)]
#[serde(
	bound(serialize = "E: serde::ser::Serialize"),
	bound(deserialize = "E: serde::de::Deserialize<'de>")
)]
pub struct FnMut<E, F> {
	env: E,
	addr: usize,
	#[serde(skip)]
	marker: marker::PhantomData<F>,
}
impl<E, T> FnMut<E, T> {
	#[doc(hidden)]
	#[inline(always)]
	pub fn private_construct(env: E, addr: usize, _: &T) -> FnMut<E, T> {
		FnMut {
			env,
			addr,
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
		unsafe { mem::transmute::<usize, fn(&mut E, T) -> F::Output>(self.addr) }
			.call_once((&mut self.env, args))
	}
}
impl<E, T, F, O> ops::FnMut<T> for FnMut<E, F>
where
	F: ops::FnOnce(&mut E, T) -> O,
{
	#[inline(always)]
	extern "rust-call" fn call_mut(&mut self, args: T) -> Self::Output {
		unsafe { mem::transmute::<usize, fn(&mut E, T) -> F::Output>(self.addr) }
			.call_mut((&mut self.env, args))
	}
}
impl<E, T> Clone for FnMut<E, T>
where
	E: Clone,
{
	fn clone(&self) -> Self {
		FnMut {
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
impl<E, T> cmp::PartialOrd for FnMut<E, T>
where
	E: cmp::PartialOrd,
{
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.env
			.partial_cmp(&other.env)
			.map(|x| x.then_with(|| self.addr.cmp(&other.addr)))
	}
}
impl<E, T> cmp::Ord for FnMut<E, T>
where
	E: cmp::Ord,
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
			.field(
				unsafe { intrinsics::type_name::<T>() },
				&(self.addr as *const ()),
			)
			.finish()
	}
}

/// A struct representing a serialisable closure, created by the [Fn](macro@Fn) macro. Implements [std::ops::Fn], serde's [Serialize](serde::ser::Serialize) and [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// It is generic over `E`: a tuple of the environment variables passed to the [Fn](macro@Fn) macro; and `F`: the signature of closure as coerced to a function pointer.
///
/// See the [readme](self) for examples.
#[derive(Serialize, Deserialize)]
#[serde(
	bound(serialize = "E: serde::ser::Serialize"),
	bound(deserialize = "E: serde::de::Deserialize<'de>")
)]
pub struct Fn<E, F> {
	env: E,
	addr: usize,
	#[serde(skip)]
	marker: marker::PhantomData<F>,
}
impl<E, T> Fn<E, T> {
	#[doc(hidden)]
	#[inline(always)]
	pub fn private_construct(env: E, addr: usize, _: &T) -> Fn<E, T> {
		Fn {
			env,
			addr,
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
		unsafe { mem::transmute::<usize, fn(&E, T) -> F::Output>(self.addr) }
			.call_once((&self.env, args))
	}
}
impl<E, T, F, O> ops::FnMut<T> for Fn<E, F>
where
	F: ops::FnOnce(&E, T) -> O,
{
	#[inline(always)]
	extern "rust-call" fn call_mut(&mut self, args: T) -> Self::Output {
		unsafe { mem::transmute::<usize, fn(&E, T) -> F::Output>(self.addr) }
			.call_mut((&self.env, args))
	}
}
impl<E, T, F, O> ops::Fn<T> for Fn<E, F>
where
	F: ops::FnOnce(&E, T) -> O,
{
	#[inline(always)]
	extern "rust-call" fn call(&self, args: T) -> Self::Output {
		unsafe { mem::transmute::<usize, fn(&E, T) -> F::Output>(self.addr) }
			.call((&self.env, args))
	}
}
impl<E, T> Clone for Fn<E, T>
where
	E: Clone,
{
	fn clone(&self) -> Self {
		Fn {
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
impl<E, T> cmp::PartialOrd for Fn<E, T>
where
	E: cmp::PartialOrd,
{
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.env
			.partial_cmp(&other.env)
			.map(|x| x.then_with(|| self.addr.cmp(&other.addr)))
	}
}
impl<E, T> cmp::Ord for Fn<E, T>
where
	E: cmp::Ord,
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
			.field(
				unsafe { intrinsics::type_name::<T>() },
				&(self.addr as *const ()),
			)
			.finish()
	}
}

/// Macro that wraps a closure, evaluating to a [FnOnce](struct@FnOnce) struct that implements [std::ops::FnOnce], serde's [Serialize](serde::ser::Serialize) and [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// See the [readme](self) for examples.
#[macro_export]
#[allow_internal_unstable]
macro_rules! FnOnce {
	([$( $env:ident ),* ] move | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		let env = ($($env,)*);
		let closure = move|($($env,)*):_,($($arg,)*):($($ty,)*)|->$o { $block };
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(env,unreachable!());
		}
		let fn_ptr = closure as fn(_,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(env,unreachable!());
		}
		$crate::FnOnce::private_construct(
			env,
			fn_ptr as usize,
			&fn_ptr,
		)
	});
	// arg out
	($([$( $env:ident ),* ])? move | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		FnOnce!([$($($env),*)?] move | $($arg : $ty),* | -> $o $block )
	});
	// !arg out
	($([$( $env:ident ),* ])? move || -> $o:ty $block:block) => ({
		FnOnce!([$($($env),*)?] move | | -> $o $block )
	});
	// arg !out
	($([$( $env:ident ),* ])? move | $( $arg:ident : $ty:ty ),* | $block:expr) => ({
		FnOnce!([$($($env),*)?] move | $($arg : $ty),* | -> _ { $block } )
	});
	// !arg !out
	($([$( $env:ident ),* ])? move || $block:expr) => ({
		FnOnce!([$($($env),*)?] move | | -> _ { $block } )
	});
	// $arg out
	($([$( $env:ident ),* ])? move | $( $arg:ident ),* | -> $o:ty $block:block) => ({
		FnOnce!([$($($env),*)?] move | $($arg : _),* | -> $o $block )
	});
	// $arg !out
	($([$( $env:ident ),* ])? move | $( $arg:ident ),* | $block:expr) => ({
		FnOnce!([$($($env),*)?] move | $($arg : _),* | -> _ { $block } )
	});
	// arg out
	($([])? | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		FnOnce!([] move | $($arg : $ty),* | -> $o $block )
	});
	// !arg out
	($([])? || -> $o:ty $block:block) => ({
		FnOnce!([] move | | -> $o $block )
	});
	// arg !out
	($([])? | $( $arg:ident : $ty:ty ),* | $block:expr) => ({
		FnOnce!([] move | $($arg : $ty),* | -> _ { $block } )
	});
	// !arg !out
	($([])? || $block:expr) => ({
		FnOnce!([] move | | -> _ { $block } )
	});
	// $arg out
	($([])? | $( $arg:ident ),* | -> $o:ty $block:block) => ({
		FnOnce!([] move | $($arg : _),* | -> $o $block )
	});
	// $arg !out
	($([])? | $( $arg:ident ),* | $block:expr) => ({
		FnOnce!([] move | $($arg : _),* | -> _ { $block } )
	});
}
/// Macro that wraps a closure, evaluating to a [FnMut](struct@FnMut) struct that implements [std::ops::FnMut], serde's [Serialize](serde::ser::Serialize) and [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// See the [readme](self) for examples.
#[macro_export]
#[allow_internal_unstable]
macro_rules! FnMut {
	([$( $env:ident ),* ] move | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		let mut env = ($($env,)*);
		let closure = move|&mut ($(ref mut $env,)*):&mut _,($($arg,)*):($($ty,)*)|->$o { $block };
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(&mut env,unreachable!());
		}
		let fn_ptr = closure as fn(&mut _,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(&mut env,unreachable!());
		}
		$crate::FnMut::private_construct(
			env,
			fn_ptr as usize,
			&fn_ptr,
		)
	});
	// arg out
	($([$( $env:ident ),* ])? move | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		FnMut!([$($($env),*)?] move | $($arg : $ty),* | -> $o $block )
	});
	// !arg out
	($([$( $env:ident ),* ])? move || -> $o:ty $block:block) => ({
		FnMut!([$($($env),*)?] move | | -> $o $block )
	});
	// arg !out
	($([$( $env:ident ),* ])? move | $( $arg:ident : $ty:ty ),* | $block:expr) => ({
		FnMut!([$($($env),*)?] move | $($arg : $ty),* | -> _ { $block } )
	});
	// !arg !out
	($([$( $env:ident ),* ])? move || $block:expr) => ({
		FnMut!([$($($env),*)?] move | | -> _ { $block } )
	});
	// $arg out
	($([$( $env:ident ),* ])? move | $( $arg:ident ),* | -> $o:ty $block:block) => ({
		FnMut!([$($($env),*)?] move | $($arg : _),* | -> $o $block )
	});
	// $arg !out
	($([$( $env:ident ),* ])? move | $( $arg:ident ),* | $block:expr) => ({
		FnMut!([$($($env),*)?] move | $($arg : _),* | -> _ { $block } )
	});
	// arg out
	($([])? | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		FnMut!([] move | $($arg : $ty),* | -> $o $block )
	});
	// !arg out
	($([])? || -> $o:ty $block:block) => ({
		FnMut!([] move | | -> $o $block )
	});
	// arg !out
	($([])? | $( $arg:ident : $ty:ty ),* | $block:expr) => ({
		FnMut!([] move | $($arg : $ty),* | -> _ { $block } )
	});
	// !arg !out
	($([])? || $block:expr) => ({
		FnMut!([] move | | -> _ { $block } )
	});
	// $arg out
	($([])? | $( $arg:ident ),* | -> $o:ty $block:block) => ({
		FnMut!([] move | $($arg : _),* | -> $o $block )
	});
	// $arg !out
	($([])? | $( $arg:ident ),* | $block:expr) => ({
		FnMut!([] move | $($arg : _),* | -> _ { $block } )
	});
}
/// Macro that wraps a closure, evaluating to a [Fn](struct@Fn) struct that implements [std::ops::Fn], serde's [Serialize](serde::ser::Serialize) and [Deserialize](serde::de::DeserializeOwned), and various convenience traits.
///
/// See the [readme](self) for examples.
#[macro_export]
#[allow_internal_unstable]
macro_rules! Fn {
	([$( $env:ident ),* ] move | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		let env = ($($env,)*);
		let closure = move|&($(ref $env,)*):&_,($($arg,)*):($($ty,)*)|->$o { $block };
		if false {
			#[allow(unreachable_code)]
			let _: $o = closure(&env,unreachable!());
		}
		let fn_ptr = closure as fn(&_,($($ty,)*))->$o;
		if false {
			#[allow(unreachable_code)]
			let _: $o = fn_ptr(&env,unreachable!());
		}
		$crate::Fn::private_construct(
			env,
			fn_ptr as usize,
			&fn_ptr,
		)
	});
	// arg out
	($([$( $env:ident ),* ])? move | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		Fn!([$($($env),*)?] move | $($arg : $ty),* | -> $o $block )
	});
	// !arg out
	($([$( $env:ident ),* ])? move || -> $o:ty $block:block) => ({
		Fn!([$($($env),*)?] move | | -> $o $block )
	});
	// arg !out
	($([$( $env:ident ),* ])? move | $( $arg:ident : $ty:ty ),* | $block:expr) => ({
		Fn!([$($($env),*)?] move | $($arg : $ty),* | -> _ { $block } )
	});
	// !arg !out
	($([$( $env:ident ),* ])? move || $block:expr) => ({
		Fn!([$($($env),*)?] move | | -> _ { $block } )
	});
	// $arg out
	($([$( $env:ident ),* ])? move | $( $arg:ident ),* | -> $o:ty $block:block) => ({
		Fn!([$($($env),*)?] move | $($arg : _),* | -> $o $block )
	});
	// $arg !out
	($([$( $env:ident ),* ])? move | $( $arg:ident ),* | $block:expr) => ({
		Fn!([$($($env),*)?] move | $($arg : _),* | -> _ { $block } )
	});
	// arg out
	($([])? | $( $arg:ident : $ty:ty ),* | -> $o:ty $block:block) => ({
		Fn!([] move | $($arg : $ty),* | -> $o $block )
	});
	// !arg out
	($([])? || -> $o:ty $block:block) => ({
		Fn!([] move | | -> $o $block )
	});
	// arg !out
	($([])? | $( $arg:ident : $ty:ty ),* | $block:expr) => ({
		Fn!([] move | $($arg : $ty),* | -> _ { $block } )
	});
	// !arg !out
	($([])? || $block:expr) => ({
		Fn!([] move | | -> _ { $block } )
	});
	// $arg out
	($([])? | $( $arg:ident ),* | -> $o:ty $block:block) => ({
		Fn!([] move | $($arg : _),* | -> $o $block )
	});
	// $arg !out
	($([])? | $( $arg:ident ),* | $block:expr) => ({
		Fn!([] move | $($arg : _),* | -> _ { $block } )
	});
}

#[cfg(test)]
mod tests {
	use bincode;
	use serde;
	use serde_json;
	use std::{fmt, mem, ops};
	#[test]
	fn fn_ptr_size() {
		assert_eq!(mem::size_of::<usize>(), mem::size_of::<fn()>());
	}
	#[test]
	fn fnonce() {
		fn test<
			F: ops::FnOnce(usize, &usize, &mut usize, String, &String, &mut String) -> String
				+ serde::ser::Serialize
				+ serde::de::DeserializeOwned
				+ PartialEq
				+ Eq
				+ Clone
				+ fmt::Debug,
		>(
			f: F,
		) {
			let deserialised: F =
				serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
			let deserialised2: F = bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
			assert_eq!(f, deserialised);
			assert_eq!(deserialised, deserialised2);
			assert_eq!(f, deserialised2);
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
			test(deserialised);
			test(deserialised2);
		}
		let (a, b) = (3usize, String::from("qwerty"));
		let a = FnOnce!([a,b] move |c:_,d:&_,e:&mut _,f:String,g:&String,h:&mut String| -> String {
			let a: usize = a;
			let b: String = b;
			*e += a+c+*d;
			// *a += *e;
			*h += ((b.clone()+f.as_str()+g.as_str())).as_str();
			// *b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		});
		test(a);
		let b = FnOnce!(|a| {
			println!("{:?}", a);
		});
		b(0usize);
		let c = FnOnce!(|arg: String| {
			println!("{}", arg);
		});
		let _ = (c, c);
	}
	#[test]
	fn fnmut() {
		fn test<
			F: ops::FnMut(usize, &usize, &mut usize, String, &String, &mut String) -> String
				+ serde::ser::Serialize
				+ serde::de::DeserializeOwned
				+ PartialEq
				+ Eq
				+ Clone
				+ fmt::Debug,
		>(
			mut f: F,
		) {
			let mut deserialised: F =
				serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
			let mut deserialised2: F =
				bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
			assert_eq!(f, deserialised);
			assert_eq!(deserialised, deserialised2);
			assert_eq!(f, deserialised2);
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
			test(&mut deserialised);
			test(&mut deserialised2);
			assert_eq!(f, deserialised);
			assert_eq!(deserialised, deserialised2);
			assert_eq!(f, deserialised2);
		}
		let (a, b) = (3usize, String::from("qwerty"));
		let a = FnMut!([a,b] move |c:_,d:&_,e:&mut _,f:String,g:&String,h:&mut String| -> String {
			let b: &mut String = b;
			*e += *a+c+*d;
			*a += *e;
			*h += ((b.clone()+f.as_str()+g.as_str())).as_str();
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
		unfold(0usize, FnMut!(|acc: &mut _| Some(*acc)));
		let x = 123usize;
		let c = FnMut!([x] move|arg:String|{
			println!("{} {}", x, arg);
		});
		let _ = (c, c);
	}
	#[test]
	fn fnref() {
		fn test<
			F: ops::Fn(usize, &usize, &mut usize, String, &String, &mut String) -> String
				+ serde::ser::Serialize
				+ serde::de::DeserializeOwned
				+ PartialEq
				+ Eq
				+ Clone
				+ fmt::Debug,
		>(
			mut f: F,
		) {
			let mut deserialised: F =
				serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
			let mut deserialised2: F =
				bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
			assert_eq!(f, deserialised);
			assert_eq!(deserialised, deserialised2);
			assert_eq!(f, deserialised2);
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
					"3qwerty129abcdefghiqwertyabcdef"
				);
				a += 6;
				b += "pqr";
				assert_eq!(
					f(
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
			test(&mut f);
			test(&mut deserialised);
			test(&mut deserialised2);
			assert_eq!(f, deserialised);
			assert_eq!(deserialised, deserialised2);
			assert_eq!(f, deserialised2);
		}
		let (a, b) = (3usize, String::from("qwerty"));
		let a = Fn!([a,b] move |c:_,d:&_,e:&mut _,f:String,g:&String,h:&mut String| -> String {
			let b: &String = b;
			*e += *a+c+*d;
			// *a += *e;
			*h += ((b.clone()+f.as_str()+g.as_str())).as_str();
			// *b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		});
		test(a);
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
		unfold(0usize, Fn!(|acc: &mut _| Some(*acc)));
		let x = 123usize;
		let c = Fn!([x] move|arg:String|{
			println!("{} {}", x, arg);
		});
		let _ = (c, c);
	}
}
