# serde_closure

[![Crates.io](https://img.shields.io/crates/v/serde_closure.svg?maxAge=86400)](https://crates.io/crates/serde_closure)
[![MIT / Apache 2.0 licensed](https://img.shields.io/crates/l/serde_closure.svg?maxAge=2592000)](#License)
[![Build Status](https://dev.azure.com/alecmocatta/serde_closure/_apis/build/status/tests?branchName=master)](https://dev.azure.com/alecmocatta/serde_closure/_build/latest?branchName=master)

[Docs](https://docs.rs/serde_closure/0.1.4)

Serializable closures.

This library provides macros to wrap closures such that they can serialized and
sent between other processes running the same binary.

```rust
fn sum_of_squares(input: &[i32]) -> i32 {
	input.dist_iter()
		.map(Fn!(|&i| i * i))
		.sum()
}
```

For example, if you have multiple forks of a process, or the same binary running
on each of a cluster of machines, this library would help you to send closures
between them.

This library aims to work in as simple and un-magical a way as possible. It
currently requires nightly Rust for the `unboxed_closures` and `fn_traits`
features (rust issue [#29625](https://github.com/rust-lang/rust/issues/29625)).

 * There are three macros,
 [FnOnce](https://docs.rs/serde_closure/0.1.4/serde_closure/macro.FnOnce.html),
 [FnMut](https://docs.rs/serde_closure/0.1.4/serde_closure/macro.FnMut.html) and
 [Fn](https://docs.rs/serde_closure/0.1.4/serde_closure/macro.Fn.html),
 corresponding to the three types of Rust closure.
 * The *captured variables*, i.e. those variables that are referenced by the
 closure but are declared outside of it, must be explicitly listed.
 * There are currently some minor limitations of syntax over normal closure
 syntax, which are documented below.
 * The closure is coerced to a function pointer, which is wrapped by
 [relative::Code](https://docs.rs/relative) such that it can safely be sent
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
|a| num += a
```
```rust
let mut num = 0;
FnMut!([num] |a| *num += a)
```
Note: As this is a FnMut closure, `num` is a mutable reference, and must be
dereferenced to use.

**`move` closure, capturing `hello` and `world`:**
```rust
let hello = String::from("hello");
let mut world = String::new();
move |name| {
	world += (hello.to_uppercase() + name).as_str();
}
```
```rust
let hello = String::from("hello");
let mut world = String::new();
FnMut!([hello, world] move |name| {
	*world += (hello.to_uppercase() + name).as_str();
})
```
Note: `world` must be dereferenced to use.

## Cosmetic limitations
As visible above, there are currently some minor limitations:
 * The captured variables in FnMut and Fn closures are references, so need
to be dereferenced;
 * Compiler errors are not as helpful as normal:
```text
error[E0308]: mismatched types
...
   = note: expected type `for<..> fn(&'r mut (..), (..))`
              found type `[closure@<FnMut macros>:9:9: 10:44 my_var:_]`
```
means that `my_var` is a captured variable, but was not explicitly listed.

## License
Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE.txt](LICENSE-APACHE.txt) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT.txt](LICENSE-MIT.txt) or http://opensource.org/licenses/MIT)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
