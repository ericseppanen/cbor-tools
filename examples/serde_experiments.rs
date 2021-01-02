//!
//! This program demonstrates that serde_cbor can decode
//! indefinite-length arrays and text strings (though the
//! difference is lost).
//!

use cbor_tools::{Array, CborType, Encode, EncodeSymbolic, Indefinite, TextString};

fn decode_print(bytes: &[u8]) -> String {
    let decoded: serde_cbor::Value = serde_cbor::from_slice(bytes).unwrap();
    format!("{:?}", decoded)
}

fn main() {
    // A regular array
    let array1 = CborType::from(vec![1, 2]).encode();
    println!("array1: {}", decode_print(&array1));

    // An array encoded as indefinite length
    let a = Array::from(vec![1, 2]);
    let array2 = CborType::Indefinite(Indefinite::Array(a)).encode();
    println!("array2: {}", decode_print(&array2));

    // A string
    let string1 = CborType::from("abcdef").encode();
    println!("string1: {}", decode_print(&string1));

    // An indefinite-length string
    let parts = ["abc", "def"]
        .iter()
        .map(|s| TextString::from(*s))
        .collect();
    let string2 = CborType::Indefinite(Indefinite::TextString(parts)).encode();

    println!("string2: {}", decode_print(&string2));

    // An indefinite-length string that improperly terminates early.
    let parts = ["abc", "def", "ghi"]
        .iter()
        .map(|s| TextString::from(*s))
        .collect();
    let mut elements = CborType::Indefinite(Indefinite::TextString(parts)).encode_symbolic();
    elements.remove(3);
    let string3 = elements.encode();

    // This shouldn't decode properly;
    println!("string3: {:#?}", decode_print(&string3));
}
