use std::{convert::TryFrom, ops::Deref};

use half::f16;

pub mod format;

/// CBOR Integer type
///
/// CBOR major type 0: unsigned integer
/// CBOR major type 1: negative integer
#[derive(Debug, Clone, PartialEq)]
pub enum Integer {
    U5(ZeroTo23), // FIXME: use some bit-field crate?
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    // Because negative integers have a broader range than
    // signed integers, they are pre-encoded as a negative
    // offset from -1.
    // FIXME: need a custom Debug implementation that understands
    // the offset, otherwise this will be confusing.
    N5(ZeroTo23),
    N8(u8),
    N16(u16),
    N32(u32),
    N64(u64),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct ZeroTo23(u8);

impl ZeroTo23 {
    pub fn new(x: u8) -> Self {
        if x >= 24 {
            panic!("too big for ZeroTo23::new()");
        }
        ZeroTo23(x)
    }
}

impl Deref for ZeroTo23 {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        if self.0 >= 24 {
            panic!("ZeroTo23 is out of range");
        }
        &self.0
    }
}

impl From<u8> for Integer {
    fn from(x: u8) -> Self {
        if x < 24 {
            Integer::U5(ZeroTo23::new(x))
        } else {
            Integer::U8(x)
        }
    }
}

impl From<u16> for Integer {
    fn from(x: u16) -> Self {
        if x < 0x100 {
            Integer::from(x as u8)
        } else {
            Integer::U16(x)
        }
    }
}

impl From<u32> for Integer {
    fn from(x: u32) -> Self {
        if x < 0x100 {
            Integer::from(x as u8)
        } else if x < 0x10000 {
            Integer::from(x as u16)
        } else {
            Integer::U32(x)
        }
    }
}

impl From<u64> for Integer {
    fn from(x: u64) -> Self {
        if x < 0x100 {
            Integer::from(x as u8)
        } else if x < 0x10000 {
            Integer::from(x as u16)
        } else if x < 0x100000000 {
            Integer::from(x as u32)
        } else {
            Integer::U64(x)
        }
    }
}

impl From<i8> for Integer {
    fn from(x: i8) -> Self {
        if x >= 0 {
            Integer::from(x as u8)
        } else if x > -25 {
            Integer::N5(ZeroTo23::new((-1 - x) as u8))
        } else {
            Integer::N8((-1 - x) as u8)
        }
    }
}

impl From<i16> for Integer {
    fn from(x: i16) -> Self {
        if x >= 0 {
            Integer::from(x as u16)
        } else if x > -0x101 {
            Integer::N8((-1 - x) as u8)
        } else {
            Integer::N16((-1 - x) as u16)
        }
    }
}

impl From<i32> for Integer {
    fn from(x: i32) -> Self {
        if x >= 0 {
            Integer::from(x as u16)
        } else if x > -0x101 {
            Integer::N8((-1 - x) as u8)
        } else if x > -0x10001 {
            Integer::N16((-1 - x) as u16)
        } else {
            Integer::N32((-1 - x) as u32)
        }
    }
}

impl From<i64> for Integer {
    fn from(x: i64) -> Self {
        if x >= 0 {
            Integer::from(x as u16)
        } else if x > -0x101 {
            Integer::N8((-1 - x) as u8)
        } else if x > -0x10001 {
            Integer::N16((-1 - x) as u16)
        } else if x > -0x100000001 {
            Integer::N32((-1 - x) as u32)
        } else {
            Integer::N64((-1 - x) as u64)
        }
    }
}

// In CBOR, only 64-bit positive and negative integers are supported.
// If your integer is out of this range, use a byte-stream instead.
#[derive(Clone, Copy, Debug)]
pub struct IntOverflowError;

impl TryFrom<i128> for Integer {
    type Error = IntOverflowError;

    fn try_from(value: i128) -> Result<Self, Self::Error> {
        if value > u64::MAX as i128 {
            Err(IntOverflowError)
        } else if value > 0 {
            Ok(Integer::from(value as u64))
        } else if value > i64::MIN as i128 {
            Ok(Integer::from(value as i64))
        } else if value < -18446744073709551616 {
            // won't fit in 64 bits after offset from -1
            Err(IntOverflowError)
        } else {
            // transform to offset from -1
            let nvalue = -1 - value;
            // Value is negative, and is outside the range
            // that i64 can store. So we know that the
            // canonical representation needs 8 bytes.
            Ok(Integer::N64(nvalue as u64))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteString {
    DefLen(Vec<u8>),
    IndefLen(Vec<Vec<u8>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TextString {
    DefLen(String),
    IndefLen(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
    pub indefinite: bool,
    pub items: Vec<CborType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map {
    pub indefinite: bool,
    pub items: Vec<CborType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Float {
    F16(f16),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CborType {
    Null,
    Undefined,
    Bool(bool),
    Integer(Integer),
    ByteString(ByteString),
    TextString(TextString),
    Array(Array),
    Map(Map),
    Tagged(Box<CborType>),
    Float(Float),
}

impl<T> From<T> for CborType
where
    T: Into<Integer>,
{
    fn from(x: T) -> Self {
        CborType::Integer(x.into())
    }
}

pub trait Canonical {
    fn is_canonical(&self) -> bool;
    fn to_canonical(&self) -> Self; // or Cow<Self> ?
}

pub trait Encode {
    fn encode(&self) -> Vec<u8>;
}

pub trait EncodeSymbolic {
    fn encode_symbolic(&self) -> Vec<format::Element>;
}

pub trait Decode {
    fn decode(&self) -> Vec<CborType>;
}

pub trait DecodeSymbolic {
    fn decode_symbolic(&self) -> Vec<format::Element>;
}

impl EncodeSymbolic for Vec<CborType> {
    fn encode_symbolic(&self) -> Vec<format::Element> {
        self.iter()
            .map(EncodeSymbolic::encode_symbolic)
            .flatten()
            .collect()
    }
}

impl Encode for Vec<CborType> {
    fn encode(&self) -> Vec<u8> {
        self.iter()
            .map(|x| x.encode_symbolic().encode())
            .flatten()
            .collect()
    }
}

impl Encode for CborType {
    fn encode(&self) -> Vec<u8> {
        self.encode_symbolic().encode()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use hex_literal::hex;

    #[test]
    fn special_types() {
        assert_eq!(CborType::Bool(false).encode(), hex!("F4"));
        assert_eq!(CborType::Bool(true).encode(), hex!("F5"));
        assert_eq!(CborType::Null.encode(), hex!("F6"));
        assert_eq!(CborType::Undefined.encode(), hex!("F7"));
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
}
