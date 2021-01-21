//!
//! This program demonstrates that serde_cbor can decode
//! indefinite-length arrays and text strings (though the
//! difference is lost).
//!

use cbor_tools::{Array, CborType, DecodeSymbolic, Encode, EncodeSymbolic, Indefinite, TextString};
use serde::Serialize;

fn decode_print(bytes: &[u8]) -> String {
    let decoded: serde_cbor::Value = serde_cbor::from_slice(bytes).unwrap();
    format!("{:?}", decoded)
}

fn decode_with_serde_cbor() {
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

#[derive(Serialize)]
struct Struct1 {
    name: String,
    id: u32,
}

fn examine_serde_encode() {
    let my_struct1 = Struct1 {
        name: "Alice".into(),
        id: 1234,
    };
    let encoded = serde_cbor::to_vec(&my_struct1).unwrap();

    println!("my_struct1 encoded: {}", hex_fmt::HexFmt(&encoded));
    // perform a partial decode to show the encoding details.
    println!("my_struct1 symbols:");
    for element in encoded.decode_symbolic().unwrap() {
        println!("   {}", element);
    }
}

fn main() {
    decode_with_serde_cbor();
    examine_serde_encode();
}
