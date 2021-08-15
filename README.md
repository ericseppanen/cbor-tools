This crate contains useful tools for working with the CBOR encoding format.

It allows encoding arbitrary CBOR sequences, including:
- indefinite-length encoding
- non-canonical encoding of integers
- tagged types
- sequences that may fail in strict-mode decoders
- malformed sequences (for testing decoders, perhaps)

It also allows decoding all valid (and many invalid) CBOR byte sequences,
including those that may not correspond to a valid data structure or can't
be handled by `serde`.
