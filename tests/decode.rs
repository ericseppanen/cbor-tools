use cbor_tools::format::{AdnInfo, Element, ImmediateValue, Major};
use cbor_tools::{
    ByteString, CborType, Decode, DecodeError, DecodeSymbolic, Indefinite, Integer, TextString,
};
use hex_literal::hex;
use std::convert::TryFrom;

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

#[test]
fn bytestring() {
    #[track_caller]
    fn assert_bytestring(buf: &[u8], expected: &[u8]) {
        let symbolic = buf.decode_symbolic().unwrap();
        // TODO: assert correct length in adn_info + imm.
        //assert_eq!(symbolic, vec![Element::simple(Major::Bstr, ...)]);
        assert_eq!(symbolic.decode(), Ok(vec![CborType::from(expected)]));
    }

    assert_bytestring(&hex!("40"), b"");
    assert_bytestring(&hex!("4401020304"), b"\x01\x02\x03\x04");

    // indefinite length from RFC 7049: (_ h'0102', h'030405')
    let expected = vec![vec![1u8, 2], vec![3u8, 4, 5]];
    let expected = expected.into_iter().map(|x| ByteString::from(x)).collect();
    let expected = CborType::Indefinite(Indefinite::ByteString(expected));
    let buf = &hex!("5f 42 0102 43 030405 ff");
    assert_eq!(buf.decode(), Ok(vec![CborType::from(expected)]));

    // TODO:
    // bytestring that is truncated
    // bytestring with a bogus adn_info field (neither <24 nor MORE1..MORE8)
    // unterminated indefinite string
    // indefinite string with bad substring type
}

#[test]
fn textstring() {
    #[track_caller]
    fn assert_textstring(buf: &[u8], expected: &str) {
        let symbolic = buf.decode_symbolic().unwrap();
        // TODO: assert correct length in adn_info + imm.
        //assert_eq!(symbolic, vec![Element::simple(Major::Tstr, ...)]);

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
    let expected = expected.into_iter().map(|s| TextString::from(s)).collect();
    let expected = CborType::Indefinite(Indefinite::TextString(expected));
    let buf = &hex!("7f 65 7374726561 64 6d696e67 ff");
    assert_eq!(buf.decode(), Ok(vec![CborType::from(expected)]));

    // string with bad UTF-8
    let bad_input = &hex!("64 fdd00000")[..];
    assert_eq!(bad_input.decode().unwrap_err(), DecodeError::Utf8Error);

    // TODO:
    // string that is truncated
    // string with a bogus adn_info field (neither <24 nor MORE1..MORE8)
    // unterminated indefinite string
    // indefinite string with bad substring type
}

// FIXME: duplicated from encode.rs
fn make_indef_array<T>(list: Vec<T>) -> CborType
where
    T: Into<CborType>,
{
    // build a regular array struct and then cannibalize it.
    let regular_array = CborType::from(list);
    if let CborType::Array(a) = regular_array {
        CborType::Indefinite(Indefinite::Array(a))
    } else {
        unreachable!()
    }
}

#[test]
fn arrays() {
    // examples from RFC 7049
    let empty = CborType::from(Vec::<u32>::new());
    assert_eq!(hex!("80").decode(), Ok(vec![empty]));

    let nums = CborType::from(vec![1, 2, 3]);
    assert_eq!(hex!("83 010203").decode(), Ok(vec![nums]));

    let deep = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        CborType::from(vec![4, 5]),
    ];
    let def_deep = CborType::from(deep.clone());
    assert_eq!(hex!("8301820203820405").decode(), Ok(vec![def_deep]));

    let twentyfive: Vec<u32> = (1..26).into_iter().collect();
    let def_twentyfive = CborType::from(twentyfive.clone());
    let buf = hex!("98190102030405060708090a0b0c0d0e0f101112131415161718181819");
    assert_eq!(buf.decode(), Ok(vec![def_twentyfive]));

    let empty_indef = make_indef_array(Vec::<u32>::new());
    assert_eq!(hex!("9fff").decode(), Ok(vec![empty_indef]));

    let deep_indef = make_indef_array(deep);
    assert_eq!(hex!("9f01820203820405ff").decode(), Ok(vec![deep_indef]));

    let deep2 = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        make_indef_array(vec![4, 5]),
    ];
    let deep2 = make_indef_array(deep2);
    assert_eq!(hex!("9f018202039f0405ffff").decode(), Ok(vec![deep2]));

    let deep3 = vec![
        CborType::from(1),
        CborType::from(vec![2, 3]),
        make_indef_array(vec![4, 5]),
    ];
    let deep3 = CborType::from(deep3);
    assert_eq!(hex!("83018202039f0405ff").decode(), Ok(vec![deep3]));

    let deep4 = vec![
        CborType::from(1),
        make_indef_array(vec![2, 3]),
        CborType::from(vec![4, 5]),
    ];
    let deep4 = CborType::from(deep4);
    assert_eq!(hex!("83019f0203ff820405").decode(), Ok(vec![deep4]));

    let indef_twentyfive = make_indef_array(twentyfive);
    let buf = hex!("9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff");
    assert_eq!(buf.decode(), Ok(vec![dbg!(indef_twentyfive)]));

    // TODO:
    // truncated array
    // unexpected break in definite-length array
}
