use half::f16;

pub mod format;

/// CBOR Integer type
///
/// CBOR major type 0: unsigned integer
/// CBOR major type 1: negative integer
#[derive(Debug, Clone, PartialEq)]
pub enum Integer {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
}

impl From<u8> for Integer {
    fn from(x: u8) -> Self {
        Integer::U8(x)
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
        assert_eq!(CborType::from(0u8).encode(), hex!("00"));
        assert_eq!(CborType::from(1u8).encode(), hex!("01"));
        assert_eq!(CborType::from(10u8).encode(), hex!("0a"));
        assert_eq!(CborType::from(23u8).encode(), hex!("17"));
        assert_eq!(CborType::from(24u8).encode(), hex!("1818"));
        assert_eq!(CborType::from(25u8).encode(), hex!("1819"));
        assert_eq!(CborType::from(100u8).encode(), hex!("1864"));
        assert_eq!(CborType::from(100u16).encode(), hex!("1864"));
        assert_eq!(CborType::from(1000u16).encode(), hex!("1903e8"));
    }
}
