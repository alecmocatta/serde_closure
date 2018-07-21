# serde_closure

[![Crates.io](https://img.shields.io/crates/v/serde_closure.svg?style=flat-square&maxAge=86400)](https://crates.io/crates/serde_closure)
[![Apache-2.0 licensed](https://img.shields.io/crates/l/serde_closure.svg?style=flat-square&maxAge=2592000)](LICENSE.txt)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/alecmocatta/serde_closure?branch=master&svg=true)](https://ci.appveyor.com/project/alecmocatta/serde-closure)
[![Build Status](https://circleci.com/gh/alecmocatta/serde_closure/tree/master.svg?style=shield)](https://circleci.com/gh/alecmocatta/serde_closure)
[![Build Status](https://travis-ci.com/alecmocatta/serde_closure.svg?branch=master)](https://travis-ci.com/alecmocatta/serde_closure)

[Docs](https://docs.rs/serde_closure/0.1.1)

Serialisable closures.

This library provides macros to wrap closures such that they can serialised and
sent between other processes running the same binary.

```rust
fn sum_of_squares(input: &[i32]) -> i32 {
	input.dist_iter()
		.map(Fn!(|&i| i * i))
		.sum()
}
```

For example, if you have the same binary running on each of a cluster of
machines, this library would help you to send closures between them.

This library aims to work in as simple and un-magical a way as possible. It
currently requires nightly Rust for the `unboxed_closures` and `fn_traits`
features (rust issue [#29625](https://github.com/rust-lang/rust/issues/29625)).

 * There are three macros,
 [FnOnce](https://docs.rs/serde_closure/0.1.1/serde_closure/macro.FnOnce.html),
 [FnMut](https://docs.rs/serde_closure/0.1.1/serde_closure/macro.FnMut.html) and
 [Fn](https://docs.rs/serde_closure/0.1.1/serde_closure/macro.Fn.html),
 corresponding to the three types of Rust closure.
 * The *captured variables*, i.e. those variables that are referenced by the
 closure but are declared outside of it, must be explicitly listed.
 * There are currently some minor limitations of syntax over normal closure
 syntax, which are documented below.
 * The closure is coerced to a function pointer, which is wrapped by
 [relative::Pointer](https://docs.rs/relative) such that it can safely be sent
 between processes.

## Examples of wrapped closures
**Inferred, non-capturing closure:**
```rust
|a| a+1
```
```rust
FnMut!(|a| a+1)
```
**Annotated, non-capturing closure:**
```rust
|a: String| -> String { a.to_uppercase() }
```
```rust
FnMut!(|a: String| -> String { a.to_uppercase() })
```
**Inferred closure, capturing `num`:**
```rust
let mut num = 0;
move |a| num += a
```
```rust
let mut num = 0;
FnMut!([num] move |a| *num += a)
```
Note: If any variables are captured then the `move` keyword must be present. As
this is a FnMut closure, `num` is a mutable reference, and must be dereferenced
to use.

**Capturing `hello` requiring extra annotation:**
```rust
let mut hello = String::new();
move |a| {
	hello = hello.to_uppercase() + a;
	hello.clone()
}
```
```rust
let mut hello = String::new();
FnMut!([hello] move |a| {
	let hello: &mut String = hello;
	*hello = hello.to_uppercase() + a;
	hello.clone()
})
```
Note: `hello` needs its type annotated in the closure.

**Complex closure, capturing `a` and `b`:**
```rust
let (mut a, mut b) = (1usize, String::from("foo"));
move |c, d: &_, e: &mut _, f: String, g: &String, h: &mut String| {
	*e += a + c + *d;
	a += *e;
	*h += (b.clone() + f.as_str() + g.as_str()).as_str();
	b += h.as_str();
}
```
```rust
let (mut a, mut b) = (1usize, String::from("foo"));
FnMut!([a,b] move |c:_, d: &_, e: &mut _, f: String, g: &String, h: &mut String| {
	let b: &mut String = b;
	*e += *a + c + *d;
	*a += *e;
	*h += ((b.clone() + f.as_str() + g.as_str())).as_str();
	*b += h.as_str();
})
```

## Cosmetic limitations
As visible above, there are currently some limitations that often necessitate
extra annotation that you might typically expect to be redundant.
 * Type inference doesn't work as well as normal, hence extra type annotations
 might be needed;
 * The captured variables in FnMut and Fn closures are references, so need to be
 dereferenced;
 * Types cannot be annotated in the list of captured variables;
 * If any of the closure arguments are annotated with types (i.e. `|a:i32|0`)
 then all must be (though `_` can be used), and patterns can no longer be used
 (i.e. `|&a:&i32|0` will not work).
 * The `move` keyword must be present if any variables are captured.

## License
Licensed under Apache License, Version 2.0, ([LICENSE.txt](LICENSE.txt) or
http://www.apache.org/licenses/LICENSE-2.0).

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
licensed as above, without any additional terms or conditions.
