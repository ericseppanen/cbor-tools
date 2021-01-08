use cbor_tools::format::{AdnInfo, Element, ImmediateValue, Major};
use cbor_tools::DecodeSymbolic;
use hex_literal::hex;

#[test]
fn simple_values() {
    #[track_caller]
    fn assert_simple(buf: &[u8], adn_info: AdnInfo) {
        assert_eq!(
            buf.decode_symbolic().unwrap(),
            vec![Element::simple(Major::Misc, adn_info)]
        );
    }
    assert_simple(&hex!("F4"), AdnInfo::FALSE);
    assert_simple(&hex!("F5"), AdnInfo::TRUE);
    assert_simple(&hex!("F6"), AdnInfo::NULL);
    assert_simple(&hex!("F7"), AdnInfo::UNDEFINED);
}

#[test]
fn uint() {
    #[track_caller]
    fn assert_u5(buf: &[u8], num: u8) {
        assert!(num < 24);
        assert_eq!(
            buf.decode_symbolic().unwrap(),
            vec![Element::simple(Major::Uint, num.into())]
        );
    }

    #[track_caller]
    fn assert_u8(buf: &[u8], num: u8) {
        assert!(num >= 24);
        assert_eq!(
            buf.decode_symbolic().unwrap(),
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE1,
                ImmediateValue::Bytes1([num])
            )]
        );
    }

    #[track_caller]
    fn assert_u16(buf: &[u8], num: u16) {
        assert!(num >= 24);
        assert_eq!(
            buf.decode_symbolic().unwrap(),
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE2,
                ImmediateValue::Bytes2(num.to_be_bytes())
            )]
        );
    }

    #[track_caller]
    fn assert_u32(buf: &[u8], num: u32) {
        assert!(num >= 24);
        assert_eq!(
            buf.decode_symbolic().unwrap(),
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE4,
                ImmediateValue::Bytes4(num.to_be_bytes())
            )]
        );
    }

    #[track_caller]
    fn assert_u64(buf: &[u8], num: u64) {
        assert!(num >= 24);
        assert_eq!(
            buf.decode_symbolic().unwrap(),
            vec![Element::new(
                Major::Uint,
                AdnInfo::MORE8,
                ImmediateValue::Bytes8(num.to_be_bytes())
            )]
        );
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
