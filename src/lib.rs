use half::f16;
use std::fmt;
use std::{convert::TryFrom, ops::Deref};

pub mod format;

/// CBOR Integer type
///
/// CBOR major type 0: unsigned integer
/// CBOR major type 1: negative integer
#[derive(Clone, PartialEq)]
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

// A crutch to assist with debug-printing of negative integers,
// which are stored in an inconvenient format.
struct DebugNeg<T: fmt::Debug>(T);

impl<T> fmt::Debug for DebugNeg<T>
where
    T: fmt::Debug + Copy + Into<u128>,
{
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        // CBOR negative values are an offset from -1.
        let x = 1u128 + self.0.into();
        write!(formatter, "-{:?}", x)
    }
}

impl fmt::Debug for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut builder = f.debug_struct("Integer");
        match self {
            Integer::U5(x) => {
                builder.field("U5", &x.0);
            }
            Integer::U8(x) => {
                builder.field("U8", &x);
            }
            Integer::U16(x) => {
                builder.field("U16", &x);
            }
            Integer::U32(x) => {
                builder.field("U32", &x);
            }
            Integer::U64(x) => {
                builder.field("U64", &x);
            }
            Integer::N5(x) => {
                let x: u8 = **x;
                builder.field("N5", &DebugNeg(x));
            }
            Integer::N8(x) => {
                builder.field("N8", &DebugNeg(*x));
            }
            Integer::N16(x) => {
                builder.field("N16", &DebugNeg(*x));
            }
            Integer::N32(x) => {
                builder.field("N32", &DebugNeg(*x));
            }
            Integer::N64(x) => {
                builder.field("N64", &DebugNeg(*x));
            }
        }
        builder.finish()
    }
}

#[derive(Copy, Clone, PartialEq)]
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
        } else if x > -25 {
            Integer::N5(ZeroTo23::new((-1 - x) as u8))
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
            Integer::from(x as u32)
        } else if x > -25 {
            Integer::N5(ZeroTo23::new((-1 - x) as u8))
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
            Integer::from(x as u64)
        } else if x > -25 {
            Integer::N5(ZeroTo23::new((-1 - x) as u8))
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

impl From<f16> for Float {
    fn from(x: f16) -> Self {
        Float::F16(x)
    }
}

impl From<f32> for Float {
    fn from(x: f32) -> Self {
        let x16 = f16::from_f32(x);
        let x32 = f32::from(x16);
        if x32 == x {
            Float::from(x16)
        } else {
            Float::F32(x)
        }
    }
}

impl From<f64> for Float {
    fn from(x: f64) -> Self {
        let x32 = x as f32;
        let x64 = x32 as f64;
        if x64 == x {
            Float::from(x32)
        } else {
            Float::F64(x)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ByteString(Vec<u8>);

impl<T> From<T> for ByteString
where
    T: Into<Vec<u8>>,
{
    // Create a definite-length byte string.
    fn from(b: T) -> Self {
        ByteString(b.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TextString(String);

impl<T> From<T> for TextString
where
    T: Into<String>,
{
    // Create a definite-length string.
    fn from(s: T) -> Self {
        TextString(s.into())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array(pub Vec<CborType>);

impl<T> From<Vec<T>> for Array
where
    T: Into<CborType>,
{
    fn from(v: Vec<T>) -> Self {
        Array(v.into_iter().map(|x| x.into()).collect())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map(Vec<(CborType, CborType)>);

impl<K, V> From<Vec<(K, V)>> for Map
where
    K: Into<CborType>,
    V: Into<CborType>,
{
    fn from(v: Vec<(K, V)>) -> Self {
        Map(v.into_iter().map(|(k, v)| (k.into(), v.into())).collect())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Float {
    F16(f16),
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tagged {
    tag: Tag,
    child: Box<CborType>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Tag(u64);

impl Tag {
    pub const STD_DATE_TIME: Tag = Tag(0);
    pub const EPOCH_DATE_TIME: Tag = Tag(1);
    pub const POS_BIGNUM: Tag = Tag(2);
    pub const NEG_BIGNUM: Tag = Tag(3);
    pub const DECIMAL_FRACTION: Tag = Tag(4);
    pub const BIGFLOAT: Tag = Tag(5);
    pub const EXPECT_BASE64URL: Tag = Tag(21);
    pub const EXPECT_BASE64: Tag = Tag(22);
    pub const EXPECT_BASE16: Tag = Tag(23);
    pub const CBOR_DATA: Tag = Tag(24);
    pub const URI: Tag = Tag(32);
    pub const BASE64URL: Tag = Tag(33);
    pub const BASE64: Tag = Tag(34);
    pub const REGEXP: Tag = Tag(35);
    pub const MIME: Tag = Tag(36);
    pub const SELF_DESC_CBOR: Tag = Tag(55799);
}

impl Tag {
    // Use the tag value to create a new Tagged struct.
    // This saves a little typing. Instead of:
    //    Tagged::new(Tag::POS_BIGNUM, bytestring)
    // one can instead type:
    //    Tag::POS_BIGNUM.wrap(bytestring)
    pub fn wrap(self, child: CborType) -> Tagged {
        Tagged {
            tag: self,
            child: Box::new(child),
        }
    }
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
    Indefinite(Indefinite),
    Tagged(Tagged),
    Float(Float),
}

/// Indefinite-length bytestrings, textstrings, arrays, and maps.
#[derive(Debug, Clone, PartialEq)]
pub enum Indefinite {
    ByteString(Vec<ByteString>),
    TextString(Vec<TextString>),
    Array(Array),
    Map(Map),
}

impl<T> From<T> for CborType
where
    T: Into<Integer>,
{
    fn from(x: T) -> Self {
        CborType::Integer(x.into())
    }
}

impl From<f16> for CborType {
    fn from(x: f16) -> Self {
        CborType::Float(x.into())
    }
}

impl From<f32> for CborType {
    fn from(x: f32) -> Self {
        CborType::Float(x.into())
    }
}

impl From<f64> for CborType {
    fn from(x: f64) -> Self {
        CborType::Float(x.into())
    }
}

impl From<&str> for CborType {
    fn from(x: &str) -> Self {
        CborType::TextString(x.into())
    }
}

impl From<&[u8]> for CborType {
    fn from(x: &[u8]) -> Self {
        CborType::ByteString(x.into())
    }
}

impl From<ByteString> for CborType {
    fn from(x: ByteString) -> CborType {
        CborType::ByteString(x)
    }
}

impl From<TextString> for CborType {
    fn from(x: TextString) -> CborType {
        CborType::TextString(x)
    }
}

impl<T> From<Vec<T>> for CborType
where
    T: Into<CborType>,
{
    fn from(x: Vec<T>) -> CborType {
        let list: Vec<CborType> = x.into_iter().map(|i| i.into()).collect();
        CborType::Array(Array(list))
    }
}

impl<K, V> From<Vec<(K, V)>> for CborType
where
    K: Into<CborType>,
    V: Into<CborType>,
{
    fn from(x: Vec<(K, V)>) -> CborType {
        let list: Vec<(CborType, CborType)> =
            x.into_iter().map(|(k, v)| (k.into(), v.into())).collect();
        CborType::Map(Map(list))
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
