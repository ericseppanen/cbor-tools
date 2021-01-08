use cbor_tools::format::{AdnInfo, Element, ImmediateValue, Major};
use cbor_tools::{CborType, Decode, DecodeSymbolic, Integer};
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
