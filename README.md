# serde_closure

[![Crates.io](https://img.shields.io/crates/v/serde_closure.svg?maxAge=86400)](https://crates.io/crates/serde_closure)
[![MIT / Apache 2.0 licensed](https://img.shields.io/crates/l/serde_closure.svg?maxAge=2592000)](#License)
[![Build Status](https://dev.azure.com/alecmocatta/serde_closure/_apis/build/status/tests?branchName=master)](https://dev.azure.com/alecmocatta/serde_closure/_build/latest?branchName=master)

[Docs](https://docs.rs/serde_closure/0.2.8)

Serializable and debuggable closures.

This library provides macros that wrap closures to make them serializable and
debuggable.

```rust
let one = 1;
let plus_one = Fn!(|x: i32| x + one);

assert_eq!(2, plus_one(1));
println!("{:#?}", plus_one);

// prints:
// Fn<main::{{closure}} at main.rs:6:15> {
//     one: 1,
//     source: "| x : i32 | x + one",
// }
```

This library aims to work in as simple and safe a way as possible. It currently
requires nightly Rust for the `unboxed_closures` and `fn_traits` features (rust
issue [#29625](https://github.com/rust-lang/rust/issues/29625)).

 * There are three macros,
   [`FnOnce`](https://docs.rs/serde_closure/0.2.8/serde_closure/macro.FnOnce.html),
   [`FnMut`](https://docs.rs/serde_closure/0.2.8/serde_closure/macro.FnMut.html)
   and [`Fn`](https://docs.rs/serde_closure/0.2.8/serde_closure/macro.Fn.html),
   corresponding to the three types of Rust closure.
 * Wrap your closure with one of the macros and it will now implement `Copy`,
   `Clone`, `PartialEq`, `Eq`, `Hash`, `PartialOrd`, `Ord`, `Serialize`,
   `Deserialize` and `Debug`.
 * There are some minor syntax limitations, which are documented below.
 * This crate has one unavoidable but documented and sound usage of
   `unsafe`.

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
FnMut!(|a| num += a)
```

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
FnMut!(move |name| {
    world += (hello.to_uppercase() + name).as_str();
})
```

## Limitations
There are currently some minor limitations:

 * Use of types that start with a lowercase letter need might need to be
   disambiguated from variables. If you see an error like the following, fix the
   case of the type, or append it with `my_struct::<>` to disambiguate.
```text
error[E0308]: mismatched types
   --> tests/test.rs:450:4
    |
449 |       FnOnce!(move || {
    |  _____-
450 | |         my_struct;
    | |         ^^^^^^^^^ expected struct `serde_closure::internal::a_variable`, found struct `my_struct`
451 | |     });
    | |______- in this macro invocation
    |
    = note: expected type `serde_closure::internal::a_variable`
               found type `my_struct`
```

 * Use of variables that start with an uppercase letter might need to be
   disambiguated from types. If you see an error like the following, fix the
   case of the variable, or wrap it with `(MyVariable)` to disambiguate.
```text
error: imports cannot refer to local variables
   --> tests/test.rs:422:3
    |
417 |       FnOnce!(move || {
    |  _____-
418 | |         MyVariable;
    | |         ^^^^^^^^^^
419 | |     });
    | |______- in this macro invocation
    |
```

 * Functions and closures called inside the closure might need to be
   disambiguated. This can be done the same as above with `function::<>` for
   functions and `(closure)` for closures.

## Serializing between processes

Closures created by this crate are unnameable – i.e. just like normal closures,
there is no Rust syntax available with which to write the type. What this means
is that to deserialize a closure, you either need to specify the precise type
you're deserializing without naming it (which is possible but not particularly
practical), or *erase* the type by storing it in a
[trait object](https://doc.rust-lang.org/beta/book/ch17-02-trait-objects.html).

The [`serde_traitobject`](https://github.com/alecmocatta/serde_traitobject)
crate enables trait objects to be safely serialized and sent between other
processes running the same binary.

For example, if you have multiple forks of a process, or the same binary running
on each of a cluster of machines,
[`serde_traitobject`](https://github.com/alecmocatta/serde_traitobject) would
help you to send serializable closures between them. This can be done by
upcasting the closure to a `Box<dyn serde_traitobject::Fn()>`, which is
automatically serializable and deserializable with
[`serde`](https://github.com/serde-rs/serde).

## License
Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE.txt](LICENSE-APACHE.txt) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT.txt](LICENSE-MIT.txt) or http://opensource.org/licenses/MIT)

at your option.

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
