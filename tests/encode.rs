use std::convert::TryFrom;

use cbor_tools::{ByteString, CborType, Encode, Indefinite, Integer, TextString};
use hex_literal::hex;

#[test]
fn special_types() {
    assert_eq!(CborType::Bool(false).encode(), hex!("F4"));
    assert_eq!(CborType::Bool(true).encode(), hex!("F5"));
    assert_eq!(CborType::Null.encode(), hex!("F6"));
    assert_eq!(CborType::Undefined.encode(), hex!("F7"));
}

#[test]
fn text_strings() {
    // examples from RFC 7049
    assert_eq!(CborType::from("").encode(), hex!("60"));
    assert_eq!(CborType::from("a").encode(), hex!("61 61"));
    assert_eq!(CborType::from("IETF").encode(), hex!("64 49455446"));
    assert_eq!(CborType::from("\"\\").encode(), hex!("62 225c"));
    assert_eq!(CborType::from("\u{00fc}").encode(), hex!("62 c3bc"));
    assert_eq!(CborType::from("\u{6c34}").encode(), hex!("63e6b0b4"));
    // Rust doesn't accept this as valid UTF-8.
    //assert_eq!(CborType::from("\u{d800}\u{dd51}").encode(), hex!("64f0908591"));

    // A 256-byte string
    let s = String::from("12345678").repeat(32);
    assert_eq!(s.len(), 256);
    let buf = CborType::from(&s[..]).encode();
    assert_eq!(&buf[..3], hex!("79 0100"));
    assert_eq!(buf.len(), 259);
    for chunk in buf[3..].chunks(8) {
        assert_eq!(chunk, "12345678".as_bytes());
    }

    // A 16KiB string
    let s = String::from("12345678").repeat(8192);
    assert_eq!(s.len(), 65536);
    let buf = CborType::from(&s[..]).encode();
    assert_eq!(buf.len(), 65541);
    assert_eq!(&buf[..5], hex!("7a 00 01 00 00"));

    // indefinite length from RFC 7049: (_ "strea", "ming")
    let list = vec!["strea", "ming"];
    let list = list.into_iter().map(|s| TextString::from(s)).collect();
    let data = CborType::Indefinite(Indefinite::TextString(list));
    assert_eq!(data.encode(), hex!("7f 65 7374726561 64 6d696e67 ff"));
}

#[test]
fn byte_strings() {
    // examples from RFC 7049
    assert_eq!(CborType::from(&b""[..]).encode(), hex!("40"));
    assert_eq!(
        CborType::from(&b"\x01\x02\x03\x04"[..]).encode(),
        hex!("4401020304")
    );

    // A 256-byte array
    let v: Vec<u8> = (1..9).into_iter().collect::<Vec<u8>>().repeat(32);
    assert_eq!(v.len(), 256);
    let buf = CborType::from(&v[..]).encode();
    assert_eq!(&buf[..3], hex!("59 0100"));
    assert_eq!(buf.len(), 259);
    for chunk in buf[3..].chunks(8) {
        assert_eq!(chunk, b"\x01\x02\x03\x04\x05\x06\x07\x08");
    }

    // indefinite length from RFC 7049: (_ h'0102', h'030405')
    let list = vec![vec![1u8, 2], vec![3u8, 4, 5]];
    let list = list.into_iter().map(|x| ByteString::from(x)).collect();
    let data = CborType::Indefinite(Indefinite::ByteString(list));
    assert_eq!(data.encode(), hex!("5f 42 0102 43 030405 ff"));

    // indefinite example from RFC 7049 2.2.2
    let str1 = Vec::from(&b"\xaa\xbb\xcc\xdd"[..]);
    let str2 = Vec::from(&b"\xee\xff\x99"[..]);
    let list = vec![str1, str2];
    let list = list.into_iter().map(|x| ByteString::from(x)).collect();
    let data = CborType::Indefinite(Indefinite::ByteString(list));
    assert_eq!(data.encode(), hex!("5f 44 aabbccdd 43 eeff99 ff"));
}

#[test]
fn uint() {
    // examples from RFC 7049
    assert_eq!(CborType::from(0u8).encode(), hex!("00"));
    assert_eq!(CborType::from(1u8).encode(), hex!("01"));
    assert_eq!(CborType::from(10u8).encode(), hex!("0a"));
    assert_eq!(CborType::from(23u8).encode(), hex!("17"));
    assert_eq!(CborType::from(24u8).encode(), hex!("18 18"));
    assert_eq!(CborType::from(25u8).encode(), hex!("18 19"));
    assert_eq!(CborType::from(100u8).encode(), hex!("18 64"));
    assert_eq!(CborType::from(1000u16).encode(), hex!("19 03e8"));
    assert_eq!(CborType::from(1000000u32).encode(), hex!("1a 000f 4240"));
    assert_eq!(
        CborType::from(1000000000000u64).encode(),
        hex!("1b 0000 00e8 d4a5 1000")
    );
    assert_eq!(
        CborType::from(18446744073709551615u64).encode(),
        hex!("1b ffff ffff ffff ffff")
    );

    // borders between sizes
    assert_eq!(CborType::from(255_u16).encode(), hex!("18 ff"));
    assert_eq!(CborType::from(256_u16).encode(), hex!("19 0100"));
    assert_eq!(CborType::from(65535_u32).encode(), hex!("19 ffff"));
    assert_eq!(CborType::from(65536_u32).encode(), hex!("1a 0001 0000"));
    assert_eq!(
        CborType::from((1_u64 << 32) - 1).encode(),
        hex!("1a ffff ffff")
    );
    assert_eq!(
        CborType::from(1_u64 << 32).encode(),
        hex!("1b 0000 0001 0000 0000")
    );

    // when using `From`, integers are canonicalized.
    assert_eq!(CborType::from(100u16).encode(), hex!("18 64"));
    assert_eq!(CborType::from(100u32).encode(), hex!("18 64"));
    assert_eq!(CborType::from(100u64).encode(), hex!("18 64"));

    // We can make non-canonical integers by hand.
    let i = Integer::U32(100);
    assert_eq!(CborType::from(i).encode(), hex!("1a 0000 0064"));
}

#[test]
fn nint() {
    // examples from RFC 7049
    assert_eq!(CborType::from(0_i8).encode(), hex!("00"));
    assert_eq!(CborType::from(-1_i8).encode(), hex!("20"));
    assert_eq!(CborType::from(-10_i8).encode(), hex!("29"));
    assert_eq!(CborType::from(-100_i8).encode(), hex!("38 63"));
    assert_eq!(CborType::from(-1000_i16).encode(), hex!("39 03e7"));

    // borders between sizes
    assert_eq!(CborType::from(-24_i8).encode(), hex!("37"));
    assert_eq!(CborType::from(-25_i8).encode(), hex!("38 18"));
    assert_eq!(CborType::from(-256_i16).encode(), hex!("38 ff"));
    assert_eq!(CborType::from(-257_i16).encode(), hex!("39 0100"));
    assert_eq!(CborType::from(-65536_i32).encode(), hex!("39 ffff"));
    assert_eq!(CborType::from(-65537_i32).encode(), hex!("3a 0001 0000"));
    assert_eq!(CborType::from(-1_i64 << 32).encode(), hex!("3a ffff ffff"));
    assert_eq!(
        CborType::from((-1_i64 << 32) - 1).encode(),
        hex!("3b 0000 0001 0000 0000")
    );

    // conversions from i128 are fallible, because we only allow
    // values that are representable in CBOR integer types.
    let val = Integer::try_from(-18446744073709551616_i128).unwrap();
    assert_eq!(CborType::from(val).encode(), hex!("3b ffff ffff ffff ffff"));

    // Too big for Integer
    Integer::try_from((u64::MAX as i128) + 1).unwrap_err();
    Integer::try_from(-2 - (u64::MAX as i128)).unwrap_err();
}

#[test]
fn int_debug() {
    let s = format!("{:?}", Integer::from(0));
    assert_eq!(s, "Integer { U5: 0 }");

    let s = format!("{:?}", Integer::from(23));
    assert_eq!(s, "Integer { U5: 23 }");

    let s = format!("{:?}", Integer::from(24));
    assert_eq!(s, "Integer { U8: 24 }");

    let s = format!("{:?}", Integer::from(256));
    assert_eq!(s, "Integer { U16: 256 }");

    let s = format!("{:?}", Integer::from(65536));
    assert_eq!(s, "Integer { U32: 65536 }");

    let s = format!("{:?}", Integer::from(1000000000000u64));
    assert_eq!(s, "Integer { U64: 1000000000000 }");

    let s = format!("{:?}", Integer::from(-1));
    assert_eq!(s, "Integer { N5: -1 }");

    let s = format!("{:?}", Integer::from(-24));
    assert_eq!(s, "Integer { N5: -24 }");

    let s = format!("{:?}", Integer::from(-25));
    assert_eq!(s, "Integer { N8: -25 }");

    let s = format!("{:?}", Integer::from(-256));
    assert_eq!(s, "Integer { N8: -256 }");

    let s = format!("{:?}", Integer::from(-257));
    assert_eq!(s, "Integer { N16: -257 }");

    let s = format!("{:?}", Integer::from(-65536));
    assert_eq!(s, "Integer { N16: -65536 }");

    let s = format!("{:?}", Integer::from(-65537));
    assert_eq!(s, "Integer { N32: -65537 }");

    let s = format!("{:?}", Integer::from(-1i64 << 32));
    assert_eq!(s, "Integer { N32: -4294967296 }");

    let s = format!("{:?}", Integer::from((-1i64 << 32) - 1));
    assert_eq!(s, "Integer { N64: -4294967297 }");

    let s = format!("{:?}", Integer::from(-1000000000000i64));
    assert_eq!(s, "Integer { N64: -1000000000000 }");

    let s = format!("{:?}", Integer::try_from(-(u64::MAX as i128)).unwrap());
    assert_eq!(s, "Integer { N64: -18446744073709551615 }");
}

// This is very limited in usefulness, because it only allows
// heterogenous arrays (all composed of the same type).
fn make_array<T>(list: Vec<T>) -> CborType
where
    T: Into<CborType>,
{
    let v: Vec<CborType> = list.into_iter().map(|x| x.into()).collect();
    CborType::from(v)
}

#[test]
fn arrays() {
    // examples from RFC 7049
    let empty = Vec::<u32>::new();
    assert_eq!(make_array(empty).encode(), hex!("80"));

    let nums = vec![1, 2, 3];
    assert_eq!(make_array(nums).encode(), hex!("83 010203"));

    let deep = vec![
        CborType::from(1),
        make_array(vec![2, 3]),
        make_array(vec![4, 5]),
    ];
    assert_eq!(make_array(deep).encode(), hex!("8301820203820405"));

    let twentyfive: Vec<u32> = (1..26).into_iter().collect();
    let expected = hex!("98190102030405060708090a0b0c0d0e0f101112131415161718181819");
    assert_eq!(make_array(twentyfive).encode(), expected);
}

// This is very limited in usefulness, because it only allows
// heterogenous maps (all keys and values composed of the same type).
fn make_map<K, V>(list: Vec<(K, V)>) -> CborType
where
    K: Into<CborType>,
    V: Into<CborType>,
{
    let v: Vec<(CborType, CborType)> = list
        .into_iter()
        .map(|(k, v)| (k.into(), v.into()))
        .collect();
    CborType::from(v)
}

#[test]
fn maps() {
    // examples from RFC 7049
    let empty = Vec::<(i8, i8)>::new();
    assert_eq!(make_map(empty).encode(), hex!("a0"));

    let kv = vec![(1, 2), (3, 4)];
    assert_eq!(make_map(kv).encode(), hex!("a2 0102 0304"));

    let kv = vec![
        (CborType::from("a"), CborType::from(1)),
        (CborType::from("b"), make_array(vec![2, 3])),
    ];
    assert_eq!(make_map(kv).encode(), hex!("a2 6161 01 6162 820203"));

    let kv = vec![("a", "A"), ("b", "B"), ("c", "C"), ("d", "D"), ("e", "E")];
    let expected = hex!("a56161614161626142616361436164614461656145");
    assert_eq!(make_map(kv).encode(), expected);
}
