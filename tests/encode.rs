use cbor_tools::test_util::*;
use cbor_tools::{ByteString, CborType, Encode, Float, Indefinite, Integer, Tag, TextString};
use half::f16;
use hex_literal::hex;
use std::convert::TryFrom;

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
    assert_eq!(CborType::from("\u{10151}").encode(), hex!("64f0908591"));

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
    let list = list.into_iter().map(TextString::from).collect();
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
    let list = list.into_iter().map(ByteString::from).collect();
    let data = CborType::Indefinite(Indefinite::ByteString(list));
    assert_eq!(data.encode(), hex!("5f 42 0102 43 030405 ff"));

    // indefinite example from RFC 7049 2.2.2
    let str1 = Vec::from(&b"\xaa\xbb\xcc\xdd"[..]);
    let str2 = Vec::from(&b"\xee\xff\x99"[..]);
    let list = vec![str1, str2];
    let list = list.into_iter().map(ByteString::from).collect();
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

#[test]
fn arrays() {
    // examples from RFC 7049
    let empty = Vec::<u32>::new();
    assert_eq!(CborType::from(empty.clone()).encode(), hex!("80"));

    let nums = vec![1, 2, 3];
    assert_eq!(CborType::from(nums).encode(), hex!("83 010203"));

    let deep = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        CborType::from(vec![4, 5]),
    ];
    assert_eq!(
        CborType::from(deep.clone()).encode(),
        hex!("8301820203820405")
    );

    let twentyfive: Vec<u32> = (1..26).into_iter().collect();
    let expected = hex!("98190102030405060708090a0b0c0d0e0f101112131415161718181819");
    assert_eq!(CborType::from(twentyfive.clone()).encode(), expected);

    assert_eq!(make_indef_array(empty).encode(), hex!("9fff"));
    assert_eq!(make_indef_array(deep).encode(), hex!("9f01820203820405ff"));

    let deep2 = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        make_indef_array(vec![4, 5]),
    ];
    assert_eq!(
        make_indef_array(deep2).encode(),
        hex!("9f018202039f0405ffff")
    );

    let deep3 = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        make_indef_array(vec![4, 5]),
    ];
    assert_eq!(CborType::from(deep3).encode(), hex!("83018202039f0405ff"));

    let deep4 = vec![
        CborType::from(1),
        make_indef_array(vec![2, 3]),
        CborType::from(vec![4, 5]),
    ];
    assert_eq!(CborType::from(deep4).encode(), hex!("83019f0203ff820405"));

    let expected = hex!("9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff");
    assert_eq!(make_indef_array(twentyfive).encode(), expected);
}

#[test]
fn maps() {
    // examples from RFC 7049
    let empty = Vec::<(i8, i8)>::new();
    assert_eq!(CborType::from(empty).encode(), hex!("a0"));

    let kv = vec![(1, 2), (3, 4)];
    assert_eq!(CborType::from(kv).encode(), hex!("a2 0102 0304"));

    let kv = vec![
        (CborType::from("a"), CborType::from(1)),
        (CborType::from("b"), CborType::from(vec![2, 3])),
    ];
    assert_eq!(CborType::from(kv).encode(), hex!("a2 6161 01 6162 820203"));

    let kv = vec![("a", "A"), ("b", "B"), ("c", "C"), ("d", "D"), ("e", "E")];
    let expected = hex!("a56161614161626142616361436164614461656145");
    assert_eq!(CborType::from(kv).encode(), expected);

    let kv = vec![
        (CborType::from("a"), CborType::from(1)),
        (CborType::from("b"), make_indef_array(vec![2, 3])),
    ];
    assert_eq!(make_indef_map(kv).encode(), hex!("bf61610161629f0203ffff"));
}

#[test]
fn floats() {
    // examples from RFC 7049
    assert_eq!(CborType::from(0.0).encode(), hex!("f9 0000"));
    assert_eq!(CborType::from(-0.0).encode(), hex!("f9 8000"));
    assert_eq!(CborType::from(1.0).encode(), hex!("f9 3c00"));
    assert_eq!(CborType::from(1.1f64).encode(), hex!("fb3ff199999999999a"));
    assert_eq!(CborType::from(1.5).encode(), hex!("f93e00"));
    assert_eq!(CborType::from(65504.0).encode(), hex!("f97bff"));
    assert_eq!(CborType::from(100000.0).encode(), hex!("fa47c35000"));
    assert_eq!(
        CborType::from(3.4028234663852886e38).encode(),
        hex!("fa7f7fffff")
    );
    assert_eq!(
        CborType::from(1.0e+300).encode(),
        hex!("fb7e37e43c8800759c")
    );
    assert_eq!(
        CborType::from(5.960464477539063e-8).encode(),
        hex!("f90001")
    );
    assert_eq!(CborType::from(0.00006103515625).encode(), hex!("f90400"));
    assert_eq!(CborType::from(-4.0).encode(), hex!("f9c400"));
    assert_eq!(CborType::from(-4.1).encode(), hex!("fbc010666666666666"));
    assert_eq!(CborType::from(f16::INFINITY).encode(), hex!("f97c00"));
    assert_eq!(CborType::from(f16::NAN).encode(), hex!("f97e00"));
    assert_eq!(CborType::from(f16::NEG_INFINITY).encode(), hex!("f9fc00"));

    // RFC 7049 suggests that the 16-bit representation of infinity/NaN is the canonical
    // one, but then has example outputs for all three sizes.
    assert_eq!(
        CborType::Float(Float::F32(f32::INFINITY)).encode(),
        hex!("fa7f800000")
    );
    assert_eq!(
        CborType::Float(Float::F32(f32::NAN)).encode(),
        hex!("fa7fc00000")
    );
    assert_eq!(
        CborType::Float(Float::F32(f32::NEG_INFINITY)).encode(),
        hex!("faff800000")
    );
    assert_eq!(
        CborType::Float(Float::F64(f64::INFINITY)).encode(),
        hex!("fb7ff0000000000000")
    );
    assert_eq!(
        CborType::Float(Float::F64(f64::NAN)).encode(),
        hex!("fb7ff8000000000000")
    );
    assert_eq!(
        CborType::Float(Float::F64(f64::NEG_INFINITY)).encode(),
        hex!("fbfff0000000000000")
    );

    // We can deliberately encode non-canonical values.
    assert_eq!(
        CborType::Float(Float::F32(1.5)).encode(),
        hex!("fa3fc00000")
    );
    assert_eq!(
        CborType::Float(Float::F64(1.5)).encode(),
        hex!("fb3ff8000000000000")
    );
}

#[test]
fn tags() {
    // RFC 7049 2.4 (roughly)
    let bytestring = CborType::from(&[0u8; 12][..]);
    let tagged = CborType::Tagged(Tag::POS_BIGNUM.wrap(bytestring));
    assert_eq!(tagged.encode(), hex!("c2 4c 000000000000000000000000"));
}

#[test]
fn map_from() {
    let kv_list = vec![(123, "foo"), (456, "bar")];
    let map = CborType::from(kv_list);
    let cbor_bytes = map.encode();
    eprintln!("{:x?}", cbor_bytes);
    assert_eq!(cbor_bytes, hex!("a2 18 7b 63 666f6f 19 01c8 63 626172"));
}

#[test]
fn array_from() {
    // An array containing a string and an integer.
    let list = vec![CborType::from("abc"), CborType::from(123)];
    let cbor_tree = CborType::from(list);
    let cbor_bytes = cbor_tree.encode();
    eprintln!("{:x?}", cbor_bytes);
    assert_eq!(cbor_bytes, hex!("82 63 616263 18 7b"));
}
