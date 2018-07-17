#![allow(missing_docs,dead_code)]

use serde;
use std::{cmp, fmt, hash, intrinsics, marker};

pub struct RelativePtr<B: Base>(usize, marker::PhantomData<fn(B)>);
impl<B: Base> RelativePtr<B> {
	#[inline(always)]
	fn new(p: usize) -> RelativePtr<B> {
		RelativePtr(p, marker::PhantomData)
	}
	#[inline(always)]
	pub fn from_ptr(ptr: *const ()) -> RelativePtr<B> {
		let base = B::base();
		RelativePtr::new((ptr as usize).wrapping_sub(base))
	}
	#[inline(always)]
	pub fn to_ptr(&self) -> *const () {
		let base = B::base();
		base.wrapping_add(self.0) as *const ()
	}
}
impl<B: Base> Clone for RelativePtr<B> {
	fn clone(&self) -> Self {
		RelativePtr(self.0, marker::PhantomData)
	}
}
impl<B: Base> Copy for RelativePtr<B> {}
impl<B: Base> PartialEq for RelativePtr<B> {
	fn eq(&self, other: &Self) -> bool {
		self.0 == other.0
	}
}
impl<B: Base> Eq for RelativePtr<B> {}
impl<B: Base> hash::Hash for RelativePtr<B> {
	fn hash<H: hash::Hasher>(&self, state: &mut H) {
		self.0.hash(state)
	}
}
impl<B: Base> cmp::PartialOrd for RelativePtr<B> {
	fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
		self.0.partial_cmp(&other.0)
	}
}
impl<B: Base> cmp::Ord for RelativePtr<B> {
	fn cmp(&self, other: &Self) -> cmp::Ordering {
		self.0.cmp(&other.0)
	}
}
impl<B: Base> fmt::Debug for RelativePtr<B> {
	fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		f.debug_struct("RelativePtr")
			.field(unsafe { intrinsics::type_name::<B>() }, &self.0)
			.finish()
	}
}
impl<B: Base> serde::ser::Serialize for RelativePtr<B> {
	#[inline(always)]
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		<usize as serde::ser::Serialize>::serialize(&self.0, serializer)
	}
}
impl<'de, B: Base> serde::de::Deserialize<'de> for RelativePtr<B> {
	#[inline(always)]
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		<usize as serde::de::Deserialize<'de>>::deserialize(deserializer).map(RelativePtr::new)
	}
}

pub trait Base {
	fn base() -> usize;
}
pub struct Text<T: ?Sized>(marker::PhantomData<fn(T)>); // +'static
impl<T: ?Sized> Text<T> {
	#[used]
	#[inline(never)]
	fn abc() -> ! {
		unsafe { intrinsics::unreachable() };
	}
}
impl<T: ?Sized> Base for Text<T> {
	#[inline(always)]
	fn base() -> usize {
		<Text<T>>::abc as *const fn() -> usize as usize
	}
}
pub struct Data;
impl Base for Data {
	#[inline(always)]
	fn base() -> usize {
		&DATA_BASE as *const () as usize
	}
}
#[used]
static DATA_BASE: () = ();
