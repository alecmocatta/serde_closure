#![forbid(unsafe_code)]
#![warn(
	trivial_numeric_casts,
	unused_import_braces,
	unused_qualifications,
	unused_results,
	unreachable_pub,
	clippy::pedantic
)]

use serde::{de::DeserializeOwned, Serialize};
use std::{fmt::Debug, hash::Hash};

use serde_closure::{traits, Fn, FnMut, FnMutNamed, FnOnce};

FnMutNamed! {
	pub type Closure<P, E> = |self|partition=> P| -> Result<P,E>
	where
		P: Default,
		E: 'static
	{
		Ok(partition)
	}
}

trait Various:
	Clone + PartialEq + Eq + Hash + PartialOrd + Ord + Serialize + DeserializeOwned + Debug
{
}
impl<T: ?Sized> Various for T where
	T: Clone + PartialEq + Eq + Hash + PartialOrd + Ord + Serialize + DeserializeOwned + Debug
{
}

#[serde_closure::desugar]
trait Pool {
	type Pool;

	fn spawn<F, T>(&self, work: F) -> Result<T, ()>
	where
		F: traits::FnOnce(&Self::Pool) -> T + 'static;

	fn spawn2<F, T>(&self, work: F) -> Result<T, ()>
	where
		F: traits::FnOnce() -> T + 'static;

	fn spawn3<F: traits::FnOnce(&Self::Pool) -> T + 'static, T>(&self, work: F) -> Result<T, ()>;
}

fn _generalize<P: Pool>(pool: &P) {
	pool.spawn(FnOnce!(|_: &_| ())).unwrap();
	pool.spawn2(FnOnce!(|| ())).unwrap();
	pool.spawn3(FnOnce!(|_: &_| ())).unwrap();
}

#[test]
fn fnonce() {
	fn test<F, A, O>(_: F)
	where
		F: Various + traits::FnOnce<A, Output = O>,
	{
	}

	test::<_, (), ()>(FnOnce!(|| ()));

	let b = String::new();
	test::<_, (String,), ()>(FnOnce!(move |_: String| drop(b)));
}

#[test]
fn fnonce_plain() {
	fn test<F, A, O>(_: F)
	where
		F: traits::FnOnce<A, Output = O>,
	{
	}

	test::<_, (), ()>(|| ());

	let b = String::new();
	test::<_, (String,), ()>(move |_: String| drop(b));
}

#[test]
fn fnmut() {
	fn test<F, A, O>(_: F)
	where
		F: Various + traits::FnMut<A, Output = O>,
	{
	}

	test::<_, (), ()>(FnMut!(|| ()));

	let b = String::new();
	test::<_, (String,), ()>(FnMut!(move |_: String| {
		let _ = b.as_mut_str();
	}));
}

#[test]
fn fnmut_plain() {
	fn test<F, A, O>(_: F)
	where
		F: traits::FnMut<A, Output = O>,
	{
	}

	test::<_, (), ()>(|| ());

	let mut b = String::new();
	test::<_, (String,), ()>(move |_: String| {
		let _ = b.as_mut_str();
	});
}

#[test]
fn fnref() {
	fn test<F, A, O>(_: F)
	where
		F: Various + traits::Fn<A, Output = O>,
	{
	}

	test::<_, (), ()>(Fn!(|| ()));

	let b = String::new();
	test::<_, (String,), ()>(Fn!(move |_: String| {
		let _ = b.as_str();
	}));
}

#[test]
fn fnref_plain() {
	fn test<F, A, O>(_: F)
	where
		F: traits::Fn<A, Output = O>,
	{
	}

	test::<_, (), ()>(|| ());

	let b = String::new();
	test::<_, (String,), ()>(move |_: String| {
		let _ = b.as_str();
	});
}
