use cbor_tools::format::{AdnInfo, Element, ImmediateValue, Major};
use cbor_tools::test_util::*;
use cbor_tools::{
    ByteString, CborType, Decode, DecodeError, DecodeSymbolic, Float, Indefinite, Integer, Tag,
    TextString,
};
use half::f16;
use hex_literal::hex;
use std::convert::TryFrom;

#[track_caller]
fn assert_decode(buf: &[u8], expected: &CborType) {
    let decoded = buf.decode().unwrap();
    assert!(decoded.len() == 1);
    assert_eq!(decoded[0], *expected);
}

#[test]
fn simple_values() {
    #[track_caller]
    fn assert_simple(buf: &[u8], adn_info: AdnInfo, decoded: CborType) {
        let symbolic = buf.decode_symbolic().unwrap();
        assert_eq!(symbolic, vec![Element::simple(Major::Misc, adn_info)]);
        assert_eq!(symbolic.decode(), Ok(vec![decoded]));
    }
    assert_simple(&hex!("F4"), AdnInfo::FALSE, CborType::Bool(false));
    assert_simple(&hex!("F5"), AdnInfo::TRUE, CborType::Bool(true));
    assert_simple(&hex!("F6"), AdnInfo::NULL, CborType::Null);
    assert_simple(&hex!("F7"), AdnInfo::UNDEFINED, CborType::Undefined);

    // This is Simple Value 16, which is unassigned.
    // We can perform a symbolic decode, but not a full decode.
    let symbolic = hex!("F0").decode_symbolic().unwrap();
    assert_eq!(
        symbolic,
        vec![Element::simple(Major::Misc, AdnInfo::from(16))]
    );
    assert_eq!(symbolic.decode(), Err(DecodeError::UnknownSimple(16)));

    // This is Simple Value 255, which is unassigned.
    // We can perform a symbolic decode, but not a full decode.
    let symbolic = hex!("F8 FF").decode_symbolic().unwrap();
    assert_eq!(
        symbolic,
        vec![Element::new(
            Major::Misc,
            AdnInfo::MORE1,
            ImmediateValue::from(255u8)
        )]
    );
    assert_eq!(symbolic.decode(), Err(DecodeError::UnknownSimple(255)));
}

#[test]
fn uint() {
    #[track_caller]
    fn assert_u5(buf: &[u8], num: u8) {
        assert!(num < 24);
        let symbolic = buf.decode_symbolic().unwrap();
        assert_eq!(symbolic, vec![Element::simple(Major::Uint, num.into())]);
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    #[track_caller]
    fn assert_u8(buf: &[u8], num: u8) {
        assert!(num >= 24);
        let symbolic = buf.decode_symbolic().unwrap();
        assert_eq!(
            symbolic,
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE1,
                ImmediateValue::Bytes1([num])
            )]
        );
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    #[track_caller]
    fn assert_u16(buf: &[u8], num: u16) {
        assert!(num >= 24);
        let symbolic = buf.decode_symbolic().unwrap();
        assert_eq!(
            symbolic,
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE2,
                ImmediateValue::Bytes2(num.to_be_bytes())
            )]
        );
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    #[track_caller]
    fn assert_u32(buf: &[u8], num: u32) {
        assert!(num >= 24);
        let symbolic = buf.decode_symbolic().unwrap();
        assert_eq!(
            symbolic,
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE4,
                ImmediateValue::Bytes4(num.to_be_bytes())
            )]
        );
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    #[track_caller]
    fn assert_u64(buf: &[u8], num: u64) {
        assert!(num >= 24);
        let symbolic = buf.decode_symbolic().unwrap();
        assert_eq!(
            symbolic,
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE8,
                ImmediateValue::Bytes8(num.to_be_bytes())
            )]
        );
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    // examples from RFC 7049
    assert_u5(&hex!("00"), 0);
    assert_u5(&hex!("01"), 1);
    assert_u5(&hex!("0a"), 10);
    assert_u5(&hex!("17"), 23);
    assert_u8(&hex!("18 18"), 24);
    assert_u8(&hex!("18 19"), 25);
    assert_u8(&hex!("18 64"), 100);
    assert_u16(&hex!("19 03e8"), 1000);
    assert_u32(&hex!("1a 000f 4240"), 1000000);
    assert_u64(&hex!("1b 0000 00e8 d4a5 1000"), 1000000000000);
    assert_u64(&hex!("1b ffff ffff ffff ffff"), 18446744073709551615);

    // Not well-formed values:
    // An integer with "additional information = 31"
    assert_eq!(
        hex!("1f").decode_symbolic(),
        Ok(vec![Element::simple(Major::Uint, AdnInfo::BREAK)])
    );
    assert_eq!(hex!("1f").decode(), Err(DecodeError::Undecodable));
}

#[test]
fn nint() {
    #[track_caller]
    fn assert_n5(buf: &[u8], num: i8) {
        assert!((-24..=-1).contains(&num));
        let symbolic = buf.decode_symbolic().unwrap();
        let adn_info = AdnInfo::from((-1 - num) as u8);
        assert_eq!(symbolic, vec![Element::simple(Major::Nint, adn_info)]);
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    // num is an i16 to allow the full cbor negative range -1 to -256.
    #[track_caller]
    fn assert_n8(buf: &[u8], num: i16) {
        // verify the input is in the canonical range
        assert!((-256..=-25).contains(&num));
        // Perform a symbolic decode to Element.
        let symbolic = buf.decode_symbolic().unwrap();
        // Figure out what the immediate value should be
        // (mangitude of distance from -1).
        let imm = ImmediateValue::from(u8::try_from(-1 - num).unwrap());
        assert_eq!(
            symbolic,
            vec![Element::new(Major::Nint, AdnInfo::MORE1, imm)]
        );
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    #[track_caller]
    fn assert_n16(buf: &[u8], num: i32) {
        assert!((-0x10000..=-0x100).contains(&num));
        let symbolic = buf.decode_symbolic().unwrap();
        let imm = ImmediateValue::from(u16::try_from(-1 - num).unwrap());
        assert_eq!(
            symbolic,
            vec![Element::new(Major::Nint, AdnInfo::MORE2, imm)]
        );
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    #[track_caller]
    fn assert_n32(buf: &[u8], num: i64) {
        assert!((-0x100000000..=-0x10000).contains(&num));
        let symbolic = buf.decode_symbolic().unwrap();
        let imm = ImmediateValue::from(u32::try_from(-1 - num).unwrap());
        assert_eq!(
            symbolic,
            vec![Element::new(Major::Nint, AdnInfo::MORE4, imm)]
        );
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(num)]))
    }

    #[track_caller]
    fn assert_n64(buf: &[u8], num: i128) {
        assert!((-0x10000000000000000..=-0x100000000).contains(&num));
        let symbolic = buf.decode_symbolic().unwrap();
        let imm = ImmediateValue::from(u64::try_from(-1 - num).unwrap());
        assert_eq!(
            symbolic,
            vec![Element::new(Major::Nint, AdnInfo::MORE8, imm)]
        );
        let integer = Integer::try_from(num).unwrap();
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(integer)]))
    }

    // examples from RFC 7049
    assert_n5(&hex!("20"), -1);
    assert_n5(&hex!("29"), -10);
    assert_n8(&hex!("38 63"), -100);
    assert_n16(&hex!("39 03e7"), -1000);

    assert_n32(&hex!("3a 0001 0000"), -65537);
    assert_n32(&hex!("3a ffff ffff"), -1_i64 << 32);
    assert_n64(&hex!("3b 0000 0001 0000 0000"), (-1_i128 << 32) - 1);
    assert_n64(&hex!("3b ffff ffff ffff ffff"), -1_i128 << 64);
}

#[track_caller]
fn assert_length(element: &Element, expected_len: usize) {
    assert_eq!(element.get_length(), Ok(expected_len));
}

#[test]
fn bytestring() {
    #[track_caller]
    fn assert_bytestring(buf: &[u8], expected: &[u8]) {
        let symbolic = buf.decode_symbolic().unwrap();
        assert_length(&symbolic[0], expected.len());
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(expected)]));
    }

    assert_bytestring(&hex!("40"), b"");
    assert_bytestring(&hex!("4401020304"), b"\x01\x02\x03\x04");

    // indefinite length from RFC 7049: (_ h'0102', h'030405')
    let expected = vec![vec![1u8, 2], vec![3u8, 4, 5]];
    let expected = expected.into_iter().map(ByteString::from).collect();
    let expected = CborType::Indefinite(Indefinite::ByteString(expected));
    let buf = &hex!("5f 42 0102 43 030405 ff");
    assert_eq!(buf.decode(), Ok(vec![expected]));

    // A bytestring that is truncated
    assert_eq!(
        hex!("44010203").decode_symbolic(),
        Err(DecodeError::Underrun)
    );
    assert_eq!(hex!("44010203").decode(), Err(DecodeError::Underrun));

    // bytestring with a bogus adn_info field (neither <24 nor MORE1..MORE8)
    assert_eq!(
        hex!("5c01020304").decode_symbolic(),
        Err(DecodeError::Undecodable)
    );
    assert_eq!(hex!("5c01020304").decode(), Err(DecodeError::Undecodable));

    // Unterminated indefinite string can be decoded to symbols.
    assert_eq!(
        hex!("5f 42 0102").decode_symbolic(),
        Ok(vec![Element::simple(Major::Bstr, AdnInfo::INDEFINITE), {
            let mut element = Element::simple(Major::Bstr, AdnInfo::from(2));
            element.set_bytes(&[1, 2]);
            element
        }])
    );
    // Unterminated indefinite string can't be fully decoded.
    assert_eq!(hex!("5f 42 0102").decode(), Err(DecodeError::Underrun));

    // Indefinite-length string with an element that's not a bytestring
    assert_eq!(
        hex!("5f 62 2020 ff").decode(),
        Err(DecodeError::BadSubString)
    );
}

#[test]
fn textstring() {
    #[track_caller]
    fn assert_textstring(buf: &[u8], expected: &str) {
        let symbolic = buf.decode_symbolic().unwrap();
        assert_length(&symbolic[0], expected.len());
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(expected)]));
    }

    // examples from RFC 7049
    assert_textstring(&hex!("60"), "");
    assert_textstring(&hex!("61 61"), "a");
    assert_textstring(&hex!("64 49455446"), "IETF");
    assert_textstring(&hex!("62 225c"), "\"\\");
    assert_textstring(&hex!("62 c3bc"), "\u{00fc}");
    assert_textstring(&hex!("63 e6b0b4"), "\u{6c34}");
    assert_textstring(&hex!("64 f0908591"), "\u{10151}");

    // indefinite length from RFC 7049: (_ "strea", "ming")
    let expected = vec!["strea", "ming"];
    let expected = expected.into_iter().map(TextString::from).collect();
    let expected = CborType::Indefinite(Indefinite::TextString(expected));
    let buf = &hex!("7f 65 7374726561 64 6d696e67 ff");
    assert_eq!(buf.decode(), Ok(vec![expected]));

    // string with bad UTF-8
    let bad_input = &hex!("64 fdd00000")[..];
    assert_eq!(bad_input.decode().unwrap_err(), DecodeError::Utf8Error);

    // TODO:
    // string with a bogus adn_info field (neither <24 nor MORE1..MORE8)
    // unterminated indefinite string
    // indefinite string with bad substring type

    // A text string that is truncated
    assert_eq!(
        hex!("64202020").decode_symbolic(),
        Err(DecodeError::Underrun)
    );
    assert_eq!(hex!("64202020").decode(), Err(DecodeError::Underrun));

    // A text string with a bogus adn_info field (neither <24 nor MORE1..MORE8)
    assert_eq!(
        hex!("7c20202020").decode_symbolic(),
        Err(DecodeError::Undecodable)
    );
    assert_eq!(hex!("7c20202020").decode(), Err(DecodeError::Undecodable));

    // Unterminated indefinite string can be decoded to symbols.
    assert_eq!(
        hex!("7f 65 7374726561").decode_symbolic(),
        Ok(vec![Element::simple(Major::Tstr, AdnInfo::INDEFINITE), {
            let mut element = Element::simple(Major::Tstr, AdnInfo::from(5));
            element.set_bytes(b"strea");
            element
        }])
    );
    // Unterminated indefinite string can't be fully decoded.
    assert_eq!(
        hex!("7f 65 7374726561").decode(),
        Err(DecodeError::Underrun)
    );

    // Indefinite-length string with an element that's not a textstring
    assert_eq!(
        hex!("7f 42 2020 ff").decode(),
        Err(DecodeError::BadSubString)
    );
}

#[test]
fn arrays() {
    // examples from RFC 7049
    let empty = CborType::from(Vec::<u32>::new());
    assert_decode(&hex!("80"), &empty);

    let nums = CborType::from(vec![1, 2, 3]);
    assert_decode(&hex!("83 010203"), &nums);

    let deep = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        CborType::from(vec![4, 5]),
    ];
    let def_deep = CborType::from(deep.clone());
    assert_decode(&hex!("8301820203820405"), &def_deep);

    let twentyfive: Vec<u32> = (1..26).collect();
    let def_twentyfive = CborType::from(twentyfive.clone());
    let buf = hex!("98190102030405060708090a0b0c0d0e0f101112131415161718181819");
    assert_decode(&buf, &def_twentyfive);

    let empty_indef = make_indef_array(Vec::<u32>::new());
    assert_decode(&hex!("9fff"), &empty_indef);

    let deep_indef = make_indef_array(deep);
    assert_decode(&hex!("9f01820203820405ff"), &deep_indef);

    let deep2 = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        make_indef_array(vec![4, 5]),
    ];
    let deep2 = make_indef_array(deep2);
    assert_decode(&hex!("9f018202039f0405ffff"), &deep2);

    let deep3 = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        make_indef_array(vec![4, 5]),
    ];
    let deep3 = CborType::from(deep3);
    assert_decode(&hex!("83018202039f0405ff"), &deep3);

    let deep4 = vec![
        CborType::from(1),
        make_indef_array(vec![2, 3]),
        CborType::from(vec![4, 5]),
    ];
    let deep4 = CborType::from(deep4);
    assert_decode(&hex!("83019f0203ff820405"), &deep4);

    let indef_twentyfive = make_indef_array(twentyfive);
    let buf = hex!("9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff");
    assert_decode(&buf, &indef_twentyfive);

    // truncated array
    assert_eq!(hex!("83 0102").decode(), Err(DecodeError::Underrun));

    // unexpected break in definite-length array
    assert_eq!(hex!("83 0102 FF").decode(), Err(DecodeError::Break));
}

#[test]
fn maps() {
    // examples from RFC 7049
    let empty = Vec::<(i8, i8)>::new();
    let empty = CborType::from(empty);
    assert_decode(&hex!("a0"), &empty);

    let kv = CborType::from(vec![(1, 2), (3, 4)]);
    assert_decode(&hex!("a2 0102 0304"), &kv);

    let kv = vec![
        (CborType::from("a"), CborType::from(1)),
        (CborType::from("b"), CborType::from(vec![2, 3])),
    ];
    let kv = CborType::from(kv);
    assert_decode(&hex!("a2 6161 01 6162 820203"), &kv);

    let kv = vec![("a", "A"), ("b", "B"), ("c", "C"), ("d", "D"), ("e", "E")];
    let kv = CborType::from(kv);
    assert_decode(&hex!("a56161614161626142616361436164614461656145"), &kv);

    let kv = vec![
        (CborType::from("a"), CborType::from(1)),
        (CborType::from("b"), make_indef_array(vec![2, 3])),
    ];
    let kv = make_indef_map(kv);
    assert_decode(&hex!("bf61610161629f0203ffff"), &kv);

    // truncated map (even)
    assert_eq!(hex!("a2 0102").decode(), Err(DecodeError::Underrun));
    // truncated map (odd)
    assert_eq!(hex!("a2 010203").decode(), Err(DecodeError::Underrun));
    // indefinite-length map with odd number of child items
    assert_eq!(
        hex!("bf 010203 ff").decode(),
        Err(DecodeError::MapPairError)
    );
    // unexpected break in definite-length map
    assert_eq!(hex!("a2 0102 ff").decode(), Err(DecodeError::Break));
}

#[test]
fn tags() {
    // RFC 7049 2.4 (roughly)
    let bytestring = CborType::from(&[0u8; 12][..]);
    let tagged = CborType::Tagged(Tag::POS_BIGNUM.wrap(bytestring));
    assert_decode(&hex!("c2 4c 000000000000000000000000"), &tagged);

    // tag with no payload
    assert_eq!(hex!("c2").decode(), Err(DecodeError::Underrun));
}

#[test]
fn floats() {
    // examples from RFC 7049
    assert_decode(&hex!("f9 0000"), &CborType::from(0.0));

    assert_decode(&hex!("f9 8000"), &CborType::from(-0.0));
    assert_decode(&hex!("f9 3c00"), &CborType::from(1.0));
    assert_decode(&hex!("fb 3ff199999999999a"), &CborType::from(1.1f64));

    assert_decode(&hex!("f9 3e00"), &CborType::from(1.5));
    assert_decode(&hex!("f9 7bff"), &CborType::from(65504.0));
    assert_decode(&hex!("fa 47c35000"), &CborType::from(100000.0));

    assert_decode(&hex!("fa 7f7fffff"), &CborType::from(3.4028234663852886e38));
    assert_decode(&hex!("fb 7e37e43c8800759c"), &CborType::from(1.0e+300));
    assert_decode(&hex!("f9 0001"), &CborType::from(5.960464477539063e-8));

    assert_decode(&hex!("f9 0400"), &CborType::from(0.00006103515625));
    assert_decode(&hex!("f9 c400"), &CborType::from(-4.0));
    assert_decode(&hex!("fb c010666666666666"), &CborType::from(-4.1));

    assert_decode(&hex!("f9 7c00"), &CborType::from(f16::INFINITY));
    assert_decode(&hex!("f9 fc00"), &CborType::from(f16::NEG_INFINITY));
    // CborType::from or Float::from will canonicalize infinity to f16...
    assert_decode(
        &hex!("fa 7f800000"),
        &CborType::Float(Float::F32(f32::INFINITY)),
    );
    assert_decode(
        &hex!("fa ff800000"),
        &CborType::Float(Float::F32(f32::NEG_INFINITY)),
    );
    assert_decode(
        &hex!("fb 7ff0000000000000"),
        &CborType::Float(Float::F64(f64::INFINITY)),
    );
    assert_decode(
        &hex!("fb fff0000000000000"),
        &CborType::Float(Float::F64(f64::NEG_INFINITY)),
    );

    // NaNs are annoying, because they don't compare equal.
    match hex!("f9 7e00").decode().unwrap()[0] {
        CborType::Float(Float::F16(x)) => assert!(x.is_nan()),
        _ => panic!("F16 NaN"),
    }
    match hex!("fa7fc00000").decode().unwrap()[0] {
        CborType::Float(Float::F32(x)) => assert!(x.is_nan()),
        _ => panic!("F32 NaN"),
    }
    match hex!("fb7ff8000000000000").decode().unwrap()[0] {
        CborType::Float(Float::F64(x)) => assert!(x.is_nan()),
        _ => panic!("F64 NaN"),
    }
}
