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
//! # 	type Item = !;
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
//!     input.dist_iter()
//!         .map(Fn!(|&i| i * i))
//!         .sum()
//! }
//! ```
//!
//! For example, if you have multiple forks of a process, or the same binary
//! running on each of a cluster of machines, this library would help you to
//! send closures between them.
//!
//! This library aims to work in as simple, safe and un-magical a way as
//! possible. It currently requires nightly Rust for the `unboxed_closures` and
//! `fn_traits` features (rust issue
//! [#29625](https://github.com/rust-lang/rust/issues/29625)).
//!
//!  * There are three macros, [`FnOnce`](macro@FnOnce), [`FnMut`](macro@FnMut)
//!    and [`Fn`](macro@Fn), corresponding to the three types of Rust closure.
//!  * Wrap your closure with one of the macros and it will now implement
//!    `Copy`, `Clone`, `PartialEq`, `Eq`, `Hash`, `PartialOrd`, `Ord`,
//!    `Serialize`, `Deserialize` and `Debug`.
//!  * There are some minor syntax limitations, which are documented below.
//!  * This crate has one unavoidable but documented and sound usage of
//!    `unsafe`.
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
//! FnMut!(|a| num += a)
//! # )(1i32);
//! ```
//!
//! **`move` closure, capturing `hello` and `world`:**
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let hello = String::from("hello");
//! let mut world = String::new();
//! # (
//! move |name| {
//!     world += (hello.to_uppercase() + name).as_str();
//! }
//! # )("abc");
//! ```
//! ```
//! # #[macro_use] extern crate serde_closure;
//! let hello = String::from("hello");
//! let mut world = String::new();
//! # (
//! FnMut!(move |name| {
//!     world += (hello.to_uppercase() + name).as_str();
//! })
//! # )("abc");
//! ```
//!
//! # Limitations
//! There are currently some minor limitations:
//!  * Captured variables with an uppercase first letter need to be explicitly
//!    captured. If you see a panic like the following, fix the case of the
//!    variable.
//! ```text
//! thread 'main' panicked at 'A variable with an upper case first letter was implicitly captured.
//! Unfortunately due to current limitations it must be captured explicitly.
//! Please refer to the README.', tests/test.rs:205:10
//! note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace.
//! ```
//!  * Functions called inside the closure might need to be disambiguated. This
//!    also affects enum unit and tuple variants with a lowercase first letter.
//!    If you see an error like either of the following, qualify `my_function`
//!    as `self::my_function` and `my_enum_variant` as
//!    `MyEnum::my_enum_variant`.
//! ```text
//! error[E0277]: the trait bound `fn(usize) -> std::option::Option<usize> {my_function::<usize>}: fnref::_IMPL_DESERIALIZE_FOR_Fn::_serde::Serialize` is not satisfied
//!    --> tests/test.rs:327:10
//!     |
//! 314 |     fn unfold<A, St, F>(initial_state: St, f: F) -> Unfold<St, F>
//!     |        ------
//! 315 |     where
//! 316 |         F: Fn(&mut St) -> Option<A> + Serialize,
//!     |                                       --------- required by this bound in `fnref::unfold`
//! ...
//! 327 |     let _ = unfold(0_usize, Fn!(|acc: &mut _| my_function(*acc)));
//!     |             ^^^^^^ the trait `fnref::_IMPL_DESERIALIZE_FOR_Fn::_serde::Serialize` is not implemented for `fn(usize) -> std::option::Option<usize> {my_function::<usize>}`
//! ```
//! ```text
//! error[E0530]: function parameters cannot shadow tuple variants
//!    --> tests/test.rs:173:47
//!     |
//! 173 |     FnMut!(|acc: &mut _| my_enum_variant(*acc))
//!     |     ---------------------^^^^^^^^^^^^^^^-------
//!     |     |                    |
//!     |     |                    cannot be named the same as a tuple variant
//!     |     in this macro invocation
//! ```

#![doc(html_root_url = "https://docs.rs/serde_closure/0.1.5")]
#![feature(unboxed_closures, fn_traits)]
#![warn(
	missing_copy_implementations,
	missing_debug_implementations,
	missing_docs,
	trivial_casts,
	trivial_numeric_casts,
	unused_import_braces,
	unused_qualifications,
	unused_results,
	clippy::pedantic
)] // from https://github.com/rust-unofficial/patterns/blob/master/anti_patterns/deny-warnings.md
#![allow(clippy::inline_always)]

use proc_macro_hack::proc_macro_hack;
use serde::{Deserialize, Serialize};
use std::fmt::{self, Debug};

/// Macro that wraps a closure, evaluating to a [`FnOnce`](struct@FnOnce) struct
/// that implements [`std::ops::FnOnce`], serde's [`Serialize`] and
/// [`Deserialize`], and various convenience traits.
///
/// See the [readme](self) for examples.
#[proc_macro_hack(fake_call_site)]
pub use serde_closure_derive::FnOnce;

/// Macro that wraps a closure, evaluating to a [`FnMut`](struct@FnMut) struct
/// that implements [`std::ops::FnMut`], serde's [`Serialize`] and
/// [`Deserialize`], and various convenience traits.
///
/// See the [readme](self) for examples.
#[proc_macro_hack(fake_call_site)]
pub use serde_closure_derive::FnMut;

/// Macro that wraps a closure, evaluating to a [`Fn`](struct@Fn) struct that
/// implements [`std::ops::Fn`], serde's [`Serialize`] and
/// [`Deserialize`], and various convenience traits.
///
/// See the [readme](self) for examples.
#[proc_macro_hack(fake_call_site)]
pub use serde_closure_derive::Fn;

#[doc(hidden)]
pub mod internal {
	use std::ops;

	pub trait FnOnce<Args> {
		type Output;

		fn call_once(self, args: Args) -> Self::Output;
	}
	pub trait FnMut<Args>: FnOnce<Args> {
		fn call_mut(&mut self, args: Args) -> Self::Output;
	}
	pub trait Fn<Args>: FnMut<Args> {
		fn call(&self, args: Args) -> Self::Output;
	}

	impl<F, I> ops::FnOnce<I> for super::FnOnce<F>
	where
		F: FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}

	impl<F, I> ops::FnOnce<I> for super::FnMut<F>
	where
		F: FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}
	impl<F, I> ops::FnMut<I> for super::FnMut<F>
	where
		F: FnMut<I>,
	{
		#[inline(always)]
		extern "rust-call" fn call_mut(&mut self, args: I) -> Self::Output {
			self.f.call_mut(args)
		}
	}

	impl<F, I> ops::FnOnce<I> for super::Fn<F>
	where
		F: FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}
	impl<F, I> ops::FnMut<I> for super::Fn<F>
	where
		F: FnMut<I>,
	{
		#[inline(always)]
		extern "rust-call" fn call_mut(&mut self, args: I) -> Self::Output {
			self.f.call_mut(args)
		}
	}
	impl<F, I> ops::Fn<I> for super::Fn<F>
	where
		F: Fn<I>,
	{
		#[inline(always)]
		extern "rust-call" fn call(&self, args: I) -> Self::Output {
			self.f.call(args)
		}
	}
}

/// A struct representing a serializable closure, created by the
/// [`FnOnce`](macro@FnOnce) macro. Implements [`std::ops::FnOnce`], serde's [`Serialize`] and
/// [`Deserialize`], and various convenience traits.
///
/// See the [readme](self) for examples.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FnOnce<F> {
	f: F,
}
impl<F> FnOnce<F> {
	/// Internal method
	#[doc(hidden)]
	pub fn internal_new<I>(f: F) -> Self
	where
		F: internal::FnOnce<I>,
	{
		Self { f }
	}
}
impl<F> Debug for FnOnce<F>
where
	F: Debug,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		Debug::fmt(&self.f, f)
	}
}

/// A struct representing a serializable closure, created by the
/// [`FnMut`](macro@FnMut) macro. Implements [`std::ops::FnMut`], serde's [`Serialize`] and
/// [`Deserialize`], and various convenience traits.
///
/// See the [readme](self) for examples.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FnMut<F> {
	f: F,
}
impl<F> FnMut<F> {
	/// Internal method
	#[doc(hidden)]
	pub fn internal_new<I>(f: F) -> Self
	where
		F: internal::FnMut<I>,
	{
		Self { f }
	}
}
impl<F> Debug for FnMut<F>
where
	F: Debug,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		Debug::fmt(&self.f, f)
	}
}

/// A struct representing a serializable closure, created by the [`Fn`](macro@Fn)
/// macro. Implements [`std::ops::Fn`], serde's [`Serialize`]
/// and [`Deserialize`], and various convenience
/// traits.
///
/// See the [readme](self) for examples.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Fn<F> {
	f: F,
}
impl<F> Fn<F> {
	/// Internal method
	#[doc(hidden)]
	pub fn internal_new<I>(f: F) -> Self
	where
		F: internal::Fn<I>,
	{
		Self { f }
	}
}
impl<F> Debug for Fn<F>
where
	F: Debug,
{
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		Debug::fmt(&self.f, f)
	}
}
