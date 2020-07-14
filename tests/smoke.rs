#![forbid(unsafe_code)]
#![warn(
	trivial_numeric_casts,
	unused_import_braces,
	unused_qualifications,
	unused_results,
	unreachable_pub,
	clippy::pedantic
)]
#![allow(clippy::too_many_lines, clippy::many_single_char_names, unused_imports)]

use serde::{de::DeserializeOwned, Serialize};
use std::{fmt::Debug, mem::size_of};

use serde_closure::{Fn, FnMut, FnOnce};

#[cfg(nightly)]
#[test]
fn fnonce() {
	fn test<
		F: FnOnce(usize, &usize, &mut usize, String, &String, &mut String) -> String
			+ Serialize
			+ DeserializeOwned
			+ PartialEq
			+ Eq
			+ Clone
			+ Debug,
	>(
		f: F,
	) {
		let deserialized: F = serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
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
				"12qwertyghiqwertyabcdef129abcdefghiqwertyabcdef"
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
				"30qwertyghiqwertyabcdefpqrqwertyjklmno4527jklmnoghiqwertyabcdefpqrqwertyjklmno"
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
				"57qwertyghiqwertyabcdefpqrqwertyjklmnoyzqwertystuvwx7854stuvwxghiqwertyabcdefpqrqwertyjklmnoyzqwertystuvwx"
			);
		};
		test(f);
		test(deserialized);
		test(deserialized2);
	}
	let (a, b) = (3_usize, String::from("qwerty"));
	let a = FnOnce!(
		move |c, d: &_, e: &mut _, f: String, g: &String, h: &mut String| -> String {
			a = a;
			b = b;
			*e += a + c + *d;
			a += *e;
			*h += (b.clone() + f.as_str() + g.as_str()).as_str();
			b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		}
	);
	test(a);
	let b = FnOnce!(|a| {
		println!("{:?}", a);
	});
	b(0_usize);
	let c = FnOnce!(|arg: String| {
		println!("{}", arg);
	});
	let _ = (c, c);

	let reduce = String::new();
	let tasks = vec![' '];
	let tasks2 = tasks.clone();
	let c = FnOnce!(move || -> String {
		for task in tasks {
			#[allow(clippy::redundant_closure_call)]
			(|| reduce.push(task))();
		}
		reduce
	});
	assert_eq!(c(), tasks2.into_iter().collect::<String>());
}

#[cfg(nightly)]
#[test]
fn fnmut() {
	fn test<
		F: FnMut(usize, &usize, &mut usize, String, &String, &mut String) -> String
			+ Serialize
			+ DeserializeOwned
			+ PartialEq
			+ Eq
			+ Clone
			+ Debug,
	>(
		mut f: F,
	) {
		let mut deserialized: F =
			serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
		let mut deserialized2: F = bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
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
	fn unfold<A, St, F>(initial_state: St, f: F) -> Unfold<St, F>
	where
		F: FnMut(&mut St) -> Option<A> + Serialize,
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

	let (mut a, mut b) = (3_usize, String::from("qwerty"));
	{
		assert_eq!(
			FnMut!(|c: usize,
			        d: &usize,
			        e: &mut usize,
			        &_x: &usize,
			        &mut _y: &mut usize,
			        f: String,
			        g: &String,
			        h: &mut String|
			 -> String {
				*e += a + c + *d;
				// *a += *e;
				*h += (b.clone() + f.as_str() + g.as_str()).as_str();
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
	let a = FnMut!(
		move |c, d: &_, e: &mut _, f: String, g: &String, h: &mut String| -> String {
			*e += a + c + *d;
			a += *e;
			*h += (b.clone() + f.as_str() + g.as_str()).as_str();
			b += h.as_str();
			format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
		}
	);
	test(a);
	let _ = unfold(0_usize, FnMut!(|acc: &mut _| Some(*acc)));
	let x = 123_usize;
	let c = FnMut!(move |arg: String| {
		println!("{} {}", x, arg);
	});
	let _ = (c, c);
}

#[cfg(nightly)]
#[test]
fn fnref() {
	fn test<
		F: Fn(
				usize,
				&usize,
				&mut usize,
				&usize,
				&mut usize,
				String,
				&String,
				&mut String,
			) -> String
			+ Serialize
			+ DeserializeOwned
			+ PartialEq
			+ Eq
			+ Clone
			+ Debug,
	>(
		mut f: F,
	) {
		let mut deserialized: F =
			serde_json::from_str(&serde_json::to_string(&f).unwrap()).unwrap();
		let mut deserialized2: F = bincode::deserialize(&bincode::serialize(&f).unwrap()).unwrap();
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
	fn unfold<A, St, F>(initial_state: St, f: F) -> Unfold<St, F>
	where
		F: Fn(&mut St) -> Option<A> + Serialize,
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

	let (a, b) = (3_usize, String::from("qwerty"));
	{
		assert_eq!(
			Fn!(|c: usize,
			     d: &usize,
			     e: &mut usize,
			     &_x: &usize,
			     &mut _y: &mut usize,
			     f: String,
			     g: &String,
			     h: &mut String|
			 -> String {
				*e += a + c + *d;
				// *a += *e;
				*h += (b.clone() + f.as_str() + g.as_str()).as_str();
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
	let x = Fn!(move |c,
	                  d: &_,
	                  e: &mut _,
	                  &_x,
	                  &mut _y: _,
	                  f: String,
	                  g: &String,
	                  h: &mut String|
	      -> String {
		*e += a + c + *d;
		// *a += *e;
		*h += (b.clone() + f.as_str() + g.as_str()).as_str();
		// *b += h.as_str();
		format!("{}{}{}{}{}{}{}{}", a, b, c, d, e, f, g, h)
	});
	test(x);
	let _ = unfold(0_usize, Fn!(|acc: &mut _| Some(*acc)));
	let x = 123_usize;
	let c = Fn!(move |arg: String| {
		println!("{} {}", x, arg);
	});
	let _ = (c, c);
}

#[test]
fn capturing() {
	struct Foo;
	#[allow(non_camel_case_types)]
	struct foo;
	fn serialize<T: Serialize>(_t: &T) {}

	let a = String::new();
	let b = move || a;
	let c = 0_u8;
	#[rustfmt::skip]
	let closure = FnOnce!(move || {
		#![allow(path_statements, unused_results, unused_qualifications, clippy::double_parens, clippy::no_effect)]
		(b)();
		// b();
		// (b::<>)();
		c;
		// c::<>;
		size_of::<()>;
		// capturing;
		// capturing::<>;
		// (capturing);
		capturing();
		// (capturing)();
		self::capturing();
		(self::capturing)();
	});
	let _ = (closure.clone(), closure);

	let d = 0_u16;
	let e = move || d;
	// TODO: move to closure, rustfmt loses it currently though
	#[allow(clippy::double_parens, unused_results)]
	let closure = FnOnce!(move || {
		(e)();
	});
	let _ = (closure, closure);

	let f = 0_u8;
	// let FooVar = Foo;
	#[rustfmt::skip]
	let closure = FnOnce!(move || {
		#![allow(path_statements, unused_results, unused_qualifications, clippy::no_effect)]
		// (e)();
		// e();
		// (e::<>)();
		f;
		// f::<>;
		size_of::<()>;
		// capturing;
		// FooVar;
		capturing::<>;
		// (capturing);
		capturing();
		// (capturing)();
		self::capturing();
		(self::capturing)();
		Foo;
		foo::<>;
		if true {
			Some(0_u8)
		} else {
			None
		}
	});
	let _ = (closure, closure);
	serialize(&closure);
}

#[cfg(nightly)]
#[test]
fn upcast() {
	let closure = FnOnce!(|_x: &str| "test");
	let closure: Box<dyn FnOnce(&str) -> &'static str + Send + Sync> = Box::new(closure);
	let _ = closure("test");
}
