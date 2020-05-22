//! Serializable and debuggable closures.
//!
//! <p style="font-family: 'Fira Sans',sans-serif;padding:0.3em 0"><strong>
//! <a href="https://crates.io/crates/serde_closure">📦&nbsp;&nbsp;Crates.io</a>&nbsp;&nbsp;│&nbsp;&nbsp;<a href="https://github.com/alecmocatta/serde_closure">📑&nbsp;&nbsp;GitHub</a>&nbsp;&nbsp;│&nbsp;&nbsp;<a href="https://constellation.zulipchat.com/#narrow/stream/213236-subprojects">💬&nbsp;&nbsp;Chat</a>
//! </strong></p>
//!
//! This library provides macros that wrap closures to make them serializable
//! and debuggable.
//!
//! ```
//! # use serde_closure::Fn;
//! let one = 1;
//! let plus_one = Fn!(|x: i32| x + one);
//!
//! assert_eq!(2, plus_one(1));
//! println!("{:#?}", plus_one);
//!
//! // prints:
//! // Fn<main::{{closure}} at main.rs:6:15> {
//! //     one: 1,
//! //     source: "| x : i32 | x + one",
//! // }
//! ```
//!
//! This library aims to work in as simple and safe a way as possible. It
//! currently requires nightly Rust for the `unboxed_closures` and `fn_traits`
//! features (rust issue
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
//!
//!  * Use of types that start with a lowercase letter need might need to be
//!    disambiguated from variables. If you see an error like the following, fix
//!    the case of the type, or append it with `my_struct::<>` to disambiguate.
//! ```text
//! error[E0308]: mismatched types
//!    --> tests/test.rs:450:4
//!     |
//! 449 |       FnOnce!(move || {
//!     |  _____-
//! 450 | |         my_struct;
//!     | |         ^^^^^^^^^ expected struct `serde_closure::internal::a_variable`, found struct `my_struct`
//! 451 | |     });
//!     | |______- in this macro invocation
//!     |
//!     = note: expected type `serde_closure::internal::a_variable`
//!                found type `my_struct`
//! ```
//!
//!  * Use of variables that start with an uppercase letter might need to be
//!    disambiguated from types. If you see an error like the following, fix the
//!    case of the variable, or wrap it with `(MyVariable)` to disambiguate.
//! ```text
//! error: imports cannot refer to local variables
//!    --> tests/test.rs:422:3
//!     |
//! 417 |       FnOnce!(move || {
//!     |  _____-
//! 418 | |         MyVariable;
//!     | |         ^^^^^^^^^^
//! 419 | |     });
//!     | |______- in this macro invocation
//!     |
//! ```
//!
//!  * Functions and closures called inside the closure might need to be
//!    disambiguated. This can be done the same as above with `function::<>` for
//!    functions and `(closure)` for closures.
//!
//! # Serializing between processes
//!
//! Closures created by this crate are unnameable – i.e. just like normal
//! closures, there is no Rust syntax available with which to write the type.
//! What this means is that to deserialize a closure, you either need to specify
//! the precise type you're deserializing without naming it (which is possible
//! but not particularly practical), or *erase* the type by storing it in a
//! [trait object](https://doc.rust-lang.org/beta/book/ch17-02-trait-objects.html).
//!
//! The [`serde_traitobject`](https://github.com/alecmocatta/serde_traitobject)
//! crate enables trait objects to be safely serialized and sent between other
//! processes running the same binary.
//!
//! For example, if you have multiple forks of a process, or the same binary
//! running on each of a cluster of machines,
//! [`serde_traitobject`](https://github.com/alecmocatta/serde_traitobject)
//! would help you to send serializable closures between them. This can be done
//! by upcasting the closure to a `Box<dyn serde_traitobject::Fn()>`, which is
//! automatically serializable and deserializable with
//! [`serde`](https://github.com/serde-rs/serde).

#![doc(html_root_url = "https://docs.rs/serde_closure/0.2.12")]
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

/// Macro that wraps a closure, evaluating to a [`FnOnce`](structs::FnOnce)
/// struct that implements [`std::ops::FnOnce`], [`Debug`](std::fmt::Debug),
/// [`Serialize`](serde::Serialize) and [`Deserialize`](serde::Deserialize), and
/// various convenience traits.
///
/// See the [readme](self) for examples.
pub use serde_closure_derive::FnOnce;

/// Macro that wraps a closure, evaluating to a [`FnMut`](structs::FnMut) struct
/// that implements [`std::ops::FnMut`], [`Debug`](std::fmt::Debug),
/// [`Serialize`](serde::Serialize) and [`Deserialize`](serde::Deserialize), and
/// various convenience traits.
///
/// See the [readme](self) for examples.
pub use serde_closure_derive::FnMut;

/// Macro that wraps a closure, evaluating to a [`Fn`](structs::Fn) struct that
/// implements [`std::ops::Fn`], [`Debug`](std::fmt::Debug),
/// [`Serialize`](serde::Serialize) and [`Deserialize`](serde::Deserialize), and
/// various convenience traits.
///
/// See the [readme](self) for examples.
pub use serde_closure_derive::Fn;

#[doc(hidden)]
pub mod internal {
	pub use core;
	pub use serde;
	pub use std;

	use std::marker::PhantomData;

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

	#[inline(always)]
	pub fn to_phantom<T>(_t: &T) -> PhantomData<fn(T)> {
		PhantomData
	}
	#[inline(always)]
	pub fn is_phantom<T>(_t: &T, _marker: PhantomData<fn(T)>) {}

	#[allow(missing_copy_implementations, missing_debug_implementations)]
	pub struct ZeroSizedAssertion;

	#[allow(
		missing_copy_implementations,
		missing_debug_implementations,
		non_camel_case_types
	)]
	pub struct a_variable;
}

pub mod structs {
	//! Structs representing a serializable closure, created by the
	//! [`FnOnce`](macro@FnOnce), [`FnMut`](macro@FnMut) and [`Fn`](macro@Fn)
	//! macros. They implement [`std::ops::FnOnce`], [`std::ops::FnMut`] and
	//! [`std::ops::Fn`] respectively, as well as [`Debug`](std::fmt::Debug),
	//! [`Serialize`](serde::Serialize) and [`Deserialize`](serde::Deserialize),
	//! and various convenience traits.
	//!
	//! See the [readme](self) for examples.

	use serde::{Deserialize, Serialize};
	use std::{
		fmt::{self, Debug}, ops
	};

	use super::internal;

	/// A struct representing a serializable closure, created by the
	/// [`FnOnce`](macro@FnOnce) macro. Implements [`std::ops::FnOnce`],
	/// [`Debug`], [`Serialize`] and [`Deserialize`], and various convenience
	/// traits.
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
		#[inline(always)]
		pub fn internal_new<I>(f: F) -> Self
		where
			F: internal::FnOnce<I>,
		{
			Self { f }
		}
	}
	impl<F, I> ops::FnOnce<I> for FnOnce<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
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
	/// [`FnMut`](macro@FnMut) macro. Implements [`std::ops::FnMut`],
	/// [`Debug`], [`Serialize`] and [`Deserialize`], and various convenience
	/// traits.
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
		#[inline(always)]
		pub fn internal_new<I>(f: F) -> Self
		where
			F: internal::FnMut<I>,
		{
			Self { f }
		}
	}
	impl<F, I> ops::FnOnce<I> for FnMut<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}
	impl<F, I> ops::FnMut<I> for FnMut<F>
	where
		F: internal::FnMut<I>,
	{
		#[inline(always)]
		extern "rust-call" fn call_mut(&mut self, args: I) -> Self::Output {
			self.f.call_mut(args)
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

	/// A struct representing a serializable closure, created by the
	/// [`Fn`](macro@Fn) macro. Implements [`std::ops::Fn`], [`Debug`],
	/// [`Serialize`] and [`Deserialize`], and various convenience traits.
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
		#[inline(always)]
		pub fn internal_new<I>(f: F) -> Self
		where
			F: internal::Fn<I>,
		{
			Self { f }
		}
	}
	impl<F, I> ops::FnOnce<I> for Fn<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}
	impl<F, I> ops::FnMut<I> for Fn<F>
	where
		F: internal::FnMut<I>,
	{
		#[inline(always)]
		extern "rust-call" fn call_mut(&mut self, args: I) -> Self::Output {
			self.f.call_mut(args)
		}
	}
	impl<F, I> ops::Fn<I> for Fn<F>
	where
		F: internal::Fn<I>,
	{
		#[inline(always)]
		extern "rust-call" fn call(&self, args: I) -> Self::Output {
			self.f.call(args)
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
}
