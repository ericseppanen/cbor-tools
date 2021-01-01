This crate contains useful tools for working with the CBOR encoding format.

It allows encoding arbitrary CBOR sequences, including:
- indefinite-length encoding
- non-canonical encoding of integers
- tagged types
- sequences that may fail in strict-mode decoders
- malformed sequences (for testing decoders, perhaps)
