<!-- cargo-sync-readme start -->

`cbor-tools` is a toolkit for manipulating CBOR-encoded data.

**CBOR** is a data serialization format described in [RFC7049].
CBOR is a binary-friendly self-describing data encoding that has
built-in types for:
- Integers and Floating point numbers
- Arrays and Maps
- Arbitrary-length UTF-8 text strings
- Arbitrary-length bytestrings

Other crates (i.e. `serde_cbor`) provide `serde` serialization and
deserialization of native Rust data structures.

This crate provides tools for constructing and deconstructing CBOR
with fine-grained control, including:
- indefinite-length encoding
- non-canonical encoding of integers
- tagged types
- sequences that may fail in strict-mode decoders
- malformed sequences (for testing decoders, perhaps)
- `Display` of low-level CBOR-encoded data

To encode some data in CBOR, create one or more `CborType` values,
and then call `encode()` on them:

```rust
use cbor_tools::{CborType, Encode};

let my_data = vec![1, 2, 3];
let cbor_tree = CborType::from(my_data);
let cbor_bytes = cbor_tree.encode();
// cbor_bytes is a Vec<u8>
```

There is a `From<T>` implementation available for many simple types.
Additional data structures can be built by hand, like this non-homogenous
array:

```rust
use cbor_tools::{CborType, Encode};

// An array containing a string and an integer.
let list = vec![
    CborType::from("abc"),
    CborType::from(123),
];
let cbor_tree = CborType::from(list);
let cbor_bytes = cbor_tree.encode();
// cbor_bytes is a Vec<u8>
```

Decoding of arbitrary CBOR data can be performed using the `Decode`
trait.

To examine the low-level details of CBOR-encoded data, use the
`DecodeSymbolic` trait, which optionally implements `Display`
if the `display` feature is enabled.


[RFC7049]: https://tools.ietf.org/html/rfc7049

<!-- cargo-sync-readme end -->
