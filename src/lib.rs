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
//! use serde_closure::{traits::Fn, Fn};
//!
//! let one = 1;
//! let plus_one = Fn!(|x: i32| x + one);
//!
//! assert_eq!(2, plus_one.call((1,))); // this works on stable and nightly
//! // assert_eq!(2, plus_one(1));      // this only works on nightly
//! println!("{:#?}", plus_one);
//!
//! // prints:
//! // Fn<main::{{closure}} at main.rs:6:15> {
//! //     one: 1,
//! //     source: "| x : i32 | x + one",
//! // }
//! ```
//!
//! This library aims to work in as simple and safe a way as possible. On stable
//! Rust the wrapped closures implement [`traits::FnOnce`], [`traits::FnMut`]
//! and [`traits::Fn`], and on nightly Rust [`std::ops::FnOnce`],
//! [`std::ops::FnMut`] and [`std::ops::Fn`] are implemented as well using the
//! `unboxed_closures` and `fn_traits` features (rust issue
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
//! # (
//! |a| a+1
//! # )(0i32);
//! ```
//! ```
//! # use serde_closure::{traits::FnMut, FnMut};
//! # (
//! FnMut!(|a| a+1)
//! # ).call_mut((0i32,));
//! ```
//!
//! **Annotated, non-capturing closure:**
//! ```
//! # (
//! |a: String| -> String { a.to_uppercase() }
//! # )(String::from("abc"));
//! ```
//! ```
//! # use serde_closure::{traits::FnMut, FnMut};
//! # (
//! FnMut!(|a: String| -> String { a.to_uppercase() })
//! # ).call_mut((String::from("abc"),));
//! ```
//!
//! **Inferred closure, capturing `num`:**
//! ```
//! let mut num = 0;
//! # (
//! |a| num += a
//! # )(1i32);
//! ```
//! ```
//! # use serde_closure::{traits::FnMut, FnMut};
//! let mut num = 0;
//! # (
//! FnMut!(|a| num += a)
//! # ).call_mut((1i32,));
//! ```
//!
//! **`move` closure, capturing `hello` and `world`:**
//! ```
//! let hello = String::from("hello");
//! let mut world = String::new();
//! # (
//! move |name| {
//!     world += (hello.to_uppercase() + name).as_str();
//! }
//! # )("abc");
//! ```
//! ```
//! # use serde_closure::{traits::FnMut, FnMut};
//! let hello = String::from("hello");
//! let mut world = String::new();
//! # (
//! FnMut!(move |name| {
//!     world += (hello.to_uppercase() + name).as_str();
//! })
//! # ).call_mut(("abc",));
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

#![doc(html_root_url = "https://docs.rs/serde_closure/0.3.1")]
#![cfg_attr(nightly, feature(unboxed_closures, fn_traits))]
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
/// struct that implements [`traits::FnOnce`] (and [`std::ops::FnOnce`] on
/// nightly), [`Debug`](std::fmt::Debug), [`Serialize`](serde::Serialize) and
/// [`Deserialize`](serde::Deserialize), and various convenience traits.
///
/// See the [readme](self) for examples.
pub use serde_closure_derive::FnOnce;

/// Macro that wraps a closure, evaluating to a [`FnMut`](structs::FnMut) struct
/// that implements [`traits::FnMut`] (and [`std::ops::FnMut`] on nightly),
/// [`Debug`](std::fmt::Debug), [`Serialize`](serde::Serialize) and
/// [`Deserialize`](serde::Deserialize), and various convenience traits.
///
/// See the [readme](self) for examples.
pub use serde_closure_derive::FnMut;

/// Macro that wraps a closure, evaluating to a [`Fn`](structs::Fn) struct that
/// implements [`traits::Fn`] (and [`std::ops::Fn`] on nightly),
/// [`Debug`](std::fmt::Debug), [`Serialize`](serde::Serialize) and
/// [`Deserialize`](serde::Deserialize), and various convenience traits.
///
/// See the [readme](self) for examples.
pub use serde_closure_derive::Fn;

/// Attribute macro that can be applied to items to desugar trait bounds
/// `FnOnce(…) -> …`, `FnMut(…) -> …` and `Fn(…) -> …` to `FnOnce<(…), Output = …>`,
/// `FnMut<(…), Output = …>` and `Fn<(…), Output = …>`. This is just a
/// convenience to enable parenthesized arguments for non `std::ops::*` traits
/// on stable Rust.
///
/// See `tests/stable.rs` for examples.
pub use serde_closure_derive::desugar;

#[doc(hidden)]
#[macro_export]
macro_rules! FnMutNamed {
	(pub type $name:ident<$($t:ident),*> = |$self:ident $(,$env:ident: $env_type:ty)*|$($arg:pat=> $arg_type:ty),*| -> $output:ty where $($bound_ty:ident : $bound_trait:tt),* $body:block) => (
		#[derive(::serde::Serialize, ::serde::Deserialize)]
		pub struct $name<$($t),*>
		where
			$($bound_ty: $bound_trait),*
		{
			$($env: $env_type,)*
			marker: ::core::marker::PhantomData<fn() -> ($($t,)*)>,
		}
		const _: () = {
			impl<$($t),*> $name<$($t),*>
			where
				$($bound_ty: $bound_trait),*
			{
				#[allow(clippy::new_without_default)]
				pub fn new($($env: $env_type),*) -> Self {
					Self {
						$($env: $env,)*
						marker: ::core::marker::PhantomData,
					}
				}
				fn run(&mut $self, ($($arg,)*): ($($arg_type,)*)) -> $output
					$body
			}
			impl<$($t),*> Clone for $name<$($t),*>
			where
				$($bound_ty: $bound_trait,)*
				$($env_type: Clone,)*
			{
				fn clone(&self) -> Self {
					Self {
						$($env: self.$env.clone(),)*
						marker: ::core::marker::PhantomData,
					}
				}
			}
			impl<$($t),*> ::serde_closure::traits::FnOnce<($($arg_type,)*)> for $name<$($t),*>
			where
				$($bound_ty: $bound_trait),*
			{
				type Output = $output;

				fn call_once(mut self, args: ($($arg_type,)*)) -> Self::Output {
					self.run(args)
				}
			}
			impl<$($t),*> ::serde_closure::traits::FnMut<($($arg_type,)*)> for $name<$($t),*>
			where
				$($bound_ty: $bound_trait),*
			{
				fn call_mut(&mut self, args: ($($arg_type,)*)) -> Self::Output {
					unsafe { $crate::internal::transmute(self.run(args)) }
				}
			}
		};
	)
}

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

	#[allow(clippy::missing_safety_doc)]
	pub unsafe fn transmute<T, U>(e: T) -> U {
		use std::mem::{self, align_of, size_of};
		assert_eq!(
			(size_of::<T>(), align_of::<T>()),
			(size_of::<U>(), align_of::<U>())
		);
		let ret = mem::transmute_copy(&e);
		mem::forget(e);
		ret
	}
}

pub mod traits {
	//! Supertraits of [`std::ops::FnOnce`], [`std::ops::FnMut`] and
	//! [`std::ops::Fn`] that are usable on stable Rust. They are implemented
	//! by closures created by the [`FnOnce`](macro@super::FnOnce),
	//! [`FnMut`](macro@super::FnMut) and [`Fn`](macro@super::Fn) macros.
	//!
	//! See the [readme](super) for examples.

	#![allow(non_snake_case)]

	use std::ops;

	/// Supertrait of [`std::ops::FnOnce`] that is usable on stable Rust. It is
	/// implemented by closures created by the [`FnOnce`](macro@super::FnOnce)
	/// macro.
	///
	/// See the [readme](super) for examples.
	pub trait FnOnce<Args> {
		/// The returned type after the call operator is used.
		type Output;

		/// Performs the call operation.
		fn call_once(self, args: Args) -> Self::Output;
	}

	/// Supertrait of [`std::ops::FnMut`] that is usable on stable Rust. It is
	/// implemented by closures created by the [`FnMut`](macro@super::FnMut)
	/// macro.
	///
	/// See the [readme](super) for examples.
	pub trait FnMut<Args>: FnOnce<Args> {
		/// Performs the call operation.
		fn call_mut(&mut self, args: Args) -> Self::Output;
	}

	/// Supertrait of [`std::ops::Fn`] that is usable on stable Rust. It is
	/// implemented by closures created by the [`Fn`](macro@super::Fn)
	/// macro.
	///
	/// See the [readme](super) for examples.
	pub trait Fn<Args>: FnMut<Args> {
		/// Performs the call operation.
		fn call(&self, args: Args) -> Self::Output;
	}

	/// A version of the [`FnOnce`] trait intended to be used for boxed trait
	/// objects to make them callable on stable Rust.
	///
	/// ```ignore
	/// let t: Box<dyn FnOnceBox(…) -> …> = …;
	/// let output = t.call_once_box((…,));
	/// ```
	pub trait FnOnceBox<A> {
		/// The returned type after the call operator is used.
		type Output;

		/// Performs the call operation on a `Box<dyn FnOnceBox(…) -> …>`.
		fn call_once_box(self: Box<Self>, args: A) -> Self::Output;
	}
	impl<A, F> FnOnceBox<A> for F
	where
		F: FnOnce<A>,
	{
		type Output = F::Output;

		fn call_once_box(self: Box<F>, args: A) -> F::Output {
			self.call_once(args)
		}
	}

	#[cfg(not(nightly))]
	macro_rules! fn_once {
		($($t:ident)*) => {
			impl<T, $($t,)* O> FnOnce<($($t,)*)> for T
			where
				T: ops::FnOnce($($t,)*) -> O,
			{
				type Output = O;

				fn call_once(self, ($($t,)*): ($($t,)*)) -> Self::Output {
					self($($t,)*)
				}
			}
			fn_once!(@recurse $($t)*);
		};
		(@recurse $first:ident $($t:ident)*) => {
			fn_once!($($t)*);
		};
		(@recurse) => {};
	}
	#[cfg(not(nightly))]
	fn_once!(A B C D E F G H I J K L);
	#[cfg(nightly)]
	impl<T, Args> FnOnce<Args> for T
	where
		T: ops::FnOnce<Args>,
	{
		type Output = T::Output;

		fn call_once(self, args: Args) -> Self::Output {
			self.call_once(args)
		}
	}

	#[cfg(not(nightly))]
	macro_rules! fn_mut {
		($($t:ident)*) => {
			impl<T, $($t,)* O> FnMut<($($t,)*)> for T
			where
				T: ops::FnMut($($t,)*) -> O,
			{
				fn call_mut(&mut self, ($($t,)*): ($($t,)*)) -> Self::Output {
					self($($t,)*)
				}
			}
			fn_mut!(@recurse $($t)*);
		};
		(@recurse $first:ident $($t:ident)*) => {
			fn_mut!($($t)*);
		};
		(@recurse) => {};
	}
	#[cfg(not(nightly))]
	fn_mut!(A B C D E F G H I J K L);
	#[cfg(nightly)]
	impl<T, Args> FnMut<Args> for T
	where
		T: ops::FnMut<Args>,
	{
		fn call_mut(&mut self, args: Args) -> Self::Output {
			self.call_mut(args)
		}
	}

	#[cfg(not(nightly))]
	macro_rules! fn_ref {
		($($t:ident)*) => {
			impl<T, $($t,)* O> Fn<($($t,)*)> for T
			where
				T: ops::Fn($($t,)*) -> O,
			{
				fn call(&self, ($($t,)*): ($($t,)*)) -> Self::Output {
					self($($t,)*)
				}
			}
			fn_ref!(@recurse $($t)*);
		};
		(@recurse $first:ident $($t:ident)*) => {
			fn_ref!($($t)*);
		};
		(@recurse) => {};
	}
	#[cfg(not(nightly))]
	fn_ref!(A B C D E F G H I J K L);
	#[cfg(nightly)]
	impl<T, Args> Fn<Args> for T
	where
		T: ops::Fn<Args>,
	{
		fn call(&self, args: Args) -> Self::Output {
			self.call(args)
		}
	}
}

pub mod structs {
	//! Structs representing a serializable closure, created by the
	//! [`FnOnce`](macro@super::FnOnce), [`FnMut`](macro@super::FnMut) and
	//! [`Fn`](macro@super::Fn) macros. They implement [`traits::FnOnce`](super::traits::FnOnce),
	//! [`traits::FnMut`](super::traits::FnMut) and [`traits::Fn`](super::traits::Fn)
	//! respectively (and [`std::ops::FnOnce`], [`std::ops::FnMut`] and [`std::ops::Fn`]
	//! on nightly), as well as [`Debug`](std::fmt::Debug), [`Serialize`](serde::Serialize)
	//! and [`Deserialize`](serde::Deserialize), and various convenience traits.
	//!
	//! See the [readme](super) for examples.

	use serde::{Deserialize, Serialize};
	use std::fmt::{self, Debug};

	use super::internal;

	/// A struct representing a serializable closure, created by the
	/// [`FnOnce`](macro@super::FnOnce) macro. Implements [`traits::FnOnce`](super::traits::FnOnce)
	/// (and [`std::ops::FnOnce`] on nightly), [`Debug`], [`Serialize`] and
	/// [`Deserialize`], and various convenience
	/// traits.
	///
	/// See the [readme](super) for examples.
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

	#[cfg(not(nightly))]
	impl<F, I> super::traits::FnOnce<I> for FnOnce<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}
	#[cfg(nightly)]
	impl<F, I> std::ops::FnOnce<I> for FnOnce<F>
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
	/// [`FnMut`](macro@super::FnMut) macro. Implements [`traits::FnMut`](super::traits::FnMut)
	/// (and [`std::ops::FnMut`] on nightly), [`Debug`], [`Serialize`] and
	/// [`Deserialize`], and various convenience
	/// traits.
	///
	/// See the [readme](super) for examples.
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

	#[cfg(not(nightly))]
	impl<F, I> super::traits::FnOnce<I> for FnMut<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}
	#[cfg(nightly)]
	impl<F, I> std::ops::FnOnce<I> for FnMut<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}

	#[cfg(not(nightly))]
	impl<F, I> super::traits::FnMut<I> for FnMut<F>
	where
		F: internal::FnMut<I>,
	{
		#[inline(always)]
		fn call_mut(&mut self, args: I) -> Self::Output {
			self.f.call_mut(args)
		}
	}
	#[cfg(nightly)]
	impl<F, I> std::ops::FnMut<I> for FnMut<F>
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
	/// [`Fn`](macro@super::Fn) macro. Implements [`traits::Fn`](super::traits::Fn)
	/// (and [`std::ops::Fn`] on nightly), [`Debug`], [`Serialize`] and
	/// [`Deserialize`], and various convenience traits.
	///
	/// See the [readme](super) for examples.
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

	#[cfg(not(nightly))]
	impl<F, I> super::traits::FnOnce<I> for Fn<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}
	#[cfg(nightly)]
	impl<F, I> std::ops::FnOnce<I> for Fn<F>
	where
		F: internal::FnOnce<I>,
	{
		type Output = F::Output;
		#[inline(always)]
		extern "rust-call" fn call_once(self, args: I) -> Self::Output {
			self.f.call_once(args)
		}
	}

	#[cfg(not(nightly))]
	impl<F, I> super::traits::FnMut<I> for Fn<F>
	where
		F: internal::FnMut<I>,
	{
		#[inline(always)]
		fn call_mut(&mut self, args: I) -> Self::Output {
			self.f.call_mut(args)
		}
	}
	#[cfg(nightly)]
	impl<F, I> std::ops::FnMut<I> for Fn<F>
	where
		F: internal::FnMut<I>,
	{
		#[inline(always)]
		extern "rust-call" fn call_mut(&mut self, args: I) -> Self::Output {
			self.f.call_mut(args)
		}
	}

	#[cfg(not(nightly))]
	impl<F, I> super::traits::Fn<I> for Fn<F>
	where
		F: internal::Fn<I>,
	{
		#[inline(always)]
		fn call(&self, args: I) -> Self::Output {
			self.f.call(args)
		}
	}
	#[cfg(nightly)]
	impl<F, I> std::ops::Fn<I> for Fn<F>
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
