//! `cbor-tools` is a toolkit for manipulating CBOR-encoded data.
//!
//! **CBOR** is a data serialization format described in [RFC7049].
//! CBOR is a binary-friendly self-describing data encoding that has
//! built-in types for:
//! - Integers and Floating point numbers
//! - Arrays and Maps
//! - Arbitrary-length UTF-8 text strings
//! - Arbitrary-length bytestrings
//!
//! Other crates (i.e. `serde_cbor`) provide `serde` serialization and
//! deserialization of native Rust data structures.
//!
//! This crate provides tools for constructing and deconstructing CBOR
//! with fine-grained control, including:
//! - indefinite-length encoding
//! - non-canonical encoding of integers
//! - tagged types
//! - sequences that may fail in strict-mode decoders
//! - malformed sequences (for testing decoders, perhaps)
//! - `Display` of low-level CBOR-encoded data
//!
//! To encode some data in CBOR, create one or more [`CborType`] values,
//! and then call [`encode()`] on them:
//!
//! ```
//! use cbor_tools::{CborType, Encode};
//!
//! let my_data = vec![1, 2, 3];
//! let cbor_tree = CborType::from(my_data);
//! let cbor_bytes = cbor_tree.encode();
//! // cbor_bytes is a Vec<u8>
//! ```
//!
//! There is a `From<T>` implementation available for many simple types.
//! Additional data structures can be built by hand, like this non-homogenous
//! array:
//!
//! ```
//! use cbor_tools::{CborType, Encode};
//!
//! // An array containing a string and an integer.
//! let list = vec![
//!     CborType::from("abc"),
//!     CborType::from(123),
//! ];
//! let cbor_tree = CborType::from(list);
//! let cbor_bytes = cbor_tree.encode();
//! // cbor_bytes is a Vec<u8>
//! # assert_eq!(cbor_bytes, [0x82, 0x63, 0x61, 0x62, 0x63, 0x18, 0x7b]);
//! ```
//!
//! Decoding of arbitrary CBOR data can be performed using the [`Decode`]
//! trait.
//!
//! To examine the low-level details of CBOR-encoded data, use the
//! [`DecodeSymbolic`] trait, which optionally implements `Display`
//! (if the `display`) feature is enabled.
//!
//!
//! [RFC7049]: https://tools.ietf.org/html/rfc7049
//! [`encode()`]: Encode::encode
//!

#![warn(missing_docs)]
#![forbid(unsafe_code)]
#![warn(clippy::cast_possible_truncation)]

use half::f16;
use std::fmt;
use std::{convert::TryFrom, ops::Deref};

/// Specifies the exact binary format of CBOR data.
pub mod format;
#[doc(hidden)]
pub mod test_util;

#[cfg(feature = "display")]
mod display;

/// CBOR Integer type
///
/// ```
/// use cbor_tools::{CborType, Integer, Encode};
///
/// let val = Integer::from(123);
/// // This is the CBOR encoding for an 8-bit integer.
/// assert_eq!(CborType::from(val).encode(), vec![0x18, 0x7B]);
/// ```
///
/// CBOR integers will be represented in "canonical" form if the
/// `From<u8 | u32 | u64>` impls are used. Non-canonical forms can
/// be created by initializing the struct directly:
/// ```
/// # use cbor_tools::{CborType, Integer, Encode};
/// let val = Integer::U32(100);
/// assert_eq!(CborType::from(val).encode(), vec![0x1a, 0, 0, 0, 0x64]);
/// ```
///
/// Note: integers outside the range of [-2^64, 2^64-1] should be encoded
/// as byte strings instead.
#[derive(Clone, PartialEq)]
pub enum Integer {
    /// A value between 0 and 23.
    U5(ZeroTo23), // FIXME: use some bit-field crate?
    /// An 8-bit non-negative integer.
    U8(u8),
    /// A 16-bit non-negative integer.
    U16(u16),
    /// A 32-bit non-negative integer.
    U32(u32),
    /// A 64-bit non-negative integer
    U64(u64),
    // Because negative integers have a broader range than
    // signed integers, they are pre-encoded as a negative
    // offset from -1.
    // FIXME: need a custom Debug implementation that understands
    // the offset, otherwise this will be confusing.
    /// A small negative integer (between -1 and -24)
    N5(ZeroTo23),
    /// An 8-bit negative integer.
    N8(u8),
    /// A 16-bit negative integer.
    N16(u16),
    /// A 32-bit negative integer.
    N32(u32),
    /// A 64-bit negative integer.
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

/// An integer value in the range 0 to 23, inclusive.
#[derive(Copy, Clone, PartialEq)]
pub struct ZeroTo23(u8);

impl ZeroTo23 {
    /// Create a new ZeroTo23.
    ///
    /// Will panic if the input is outside the expected range.
    pub fn new(x: u8) -> Self {
        if x >= 24 {
            panic!("too big for ZeroTo23::new()");
        }
        ZeroTo23(x)
    }
}

impl Deref for ZeroTo23 {
    type Target = u8;

    /// Extract the integer value.
    ///
    /// Will panic if the stored value is outside the expected range.
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

/// The integer value was too large to be represented as a CBOR integer.
///
/// In CBOR, only 64-bit positive and negative integers are supported.
/// If your integer is out of this range, use a byte-stream instead.
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

/// A byte string.
///
/// A "byte string" in CBOR is an arbitrary length array of bytes.
///
/// ```
/// use cbor_tools::{CborType, ByteString, Encode};
///
/// let bytes = [1u8, 2, 3, 4];
/// let val = ByteString::from(&bytes[..]);
/// assert_eq!(CborType::from(val).encode(), vec![0x44, 1, 2, 3, 4]);
/// ```
///
#[derive(Debug, Clone, PartialEq)]
pub struct ByteString(pub Vec<u8>);

impl<T> From<T> for ByteString
where
    T: Into<Vec<u8>>,
{
    // Create a definite-length byte string.
    fn from(b: T) -> Self {
        ByteString(b.into())
    }
}

/// A UTF-8 text string.
///
/// ```
/// use cbor_tools::{CborType, TextString, Encode};
///
/// let name = "Foo!";
/// let val = TextString::from(name);
/// assert_eq!(CborType::from(val).encode(), vec![0x64, 0x46, 0x6f, 0x6f, 0x21]);
/// ```
///
#[derive(Debug, Clone, PartialEq)]
pub struct TextString(pub String);

impl<T> From<T> for TextString
where
    T: Into<String>,
{
    // Create a definite-length string.
    fn from(s: T) -> Self {
        TextString(s.into())
    }
}

/// An array of values.
///
/// In CBOR, arrays may have members of different types.
///
/// Use `Array::from()` to construct an array.
///
/// ```
/// # use cbor_tools::{CborType, Array, Encode};
/// let nums = vec![1, 2, 3];
/// let array = Array::from(nums);
/// assert_eq!(CborType::from(array).encode(), vec![0x83, 1, 2, 3]);
/// ```
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

/// An map of (key, value) pairs.
///
/// In CBOR, each key and value may be of different types.
///
/// Use `Map::from()` to construct a map.
///
/// ```
/// # use cbor_tools::{CborType, Map, Encode};
/// let map_pairs = vec![(1, 2), (3, 4)];
/// let map = Map::from(map_pairs);
/// assert_eq!(CborType::from(map).encode(), vec![0xa2, 1, 2, 3, 4]);
/// ```
///
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

/// A floating-point value.
///
/// Use `Float::from()` to construct a `Float`.
#[derive(Debug, Clone, PartialEq)]
pub enum Float {
    /// IEEE 754 Half-Precision Float (16 bits)
    F16(f16),
    /// IEEE 754 Single-Precision Float (32 bits)
    F32(f32),
    /// IEEE 754 Double-Precision Float (64 bits)
    F64(f64),
}

/// A tagged value.
///
/// A Tagged value contains a numeric tag, which specifies
/// some additional information, and a payload value, which
/// may be of any type (though some tags are only expected
/// to be used with particular types).
///
/// Use [`Tag::wrap`] to create a `Tagged`.
#[derive(Debug, Clone, PartialEq)]
pub struct Tagged {
    tag: Tag,
    child: Box<CborType>,
}

/// A tag value for use with [`Tagged`]
///
/// Tags are just integers; this type exists to avoid any
/// type confusion when passing them as function arguments.
///
/// See RFC 7049 2.4 for details.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Tag(u64);

#[allow(missing_docs)]
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
    /// Use the [`Tag`] value to create a new [`Tagged`] struct.
    ///
    /// This saves a little typing. Instead of:
    /// ```compile_fail
    /// # // doesn't build; there is no `new` fn.
    /// # use cbor_tools::{CborType, Tag, Tagged};
    /// # let bytestring = CborType::from(&[0u8; 12][..]);
    /// Tagged::new(Tag::POS_BIGNUM, bytestring)
    /// # ;
    /// ```
    /// one can instead type:
    /// ```
    /// # use cbor_tools::{CborType, Tag};
    /// # let bytestring = CborType::from(&[0u8; 12][..]);
    /// Tag::POS_BIGNUM.wrap(bytestring)
    /// # ;
    ///```
    pub fn wrap(self, child: CborType) -> Tagged {
        Tagged {
            tag: self,
            child: Box::new(child),
        }
    }
}

/// A CBOR value.
///
/// This enum can represent any CBOR value; see the documentation
/// of each variant for more details.
///
/// Many variants can be constructed directly using `from()`.
/// For example,
/// ```
/// # use cbor_tools::CborType;
/// let i = 42;
/// let x = CborType::from(i);
/// ```
/// produces the same value as
/// ```
/// # use cbor_tools::{CborType, Integer};
/// let i = 42;
/// let x = CborType::Integer(Integer::from(i));
/// ```
#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
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
#[allow(missing_docs)]
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

impl From<Array> for CborType {
    fn from(x: Array) -> CborType {
        CborType::Array(x)
    }
}

impl From<Map> for CborType {
    fn from(x: Map) -> CborType {
        CborType::Map(x)
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

#[doc(hidden)]
pub trait Canonical {
    fn is_canonical(&self) -> bool;
    fn to_canonical(&self) -> Self; // or Cow<Self> ?
}

/// Binary CBOR encoding.
pub trait Encode {
    /// Encode data to  bytes.
    fn encode(&self) -> Vec<u8>;
}

/// Symbolic CBOR encoding.
pub trait EncodeSymbolic {
    /// Encode data to [`format::Element`] symbols representing a CBOR encoding.
    fn encode_symbolic(&self) -> Vec<format::Element>;
}

/// An error that may occur when decoding CBOR Data.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum DecodeError {
    /// No more CBOR items are available in the input data.
    End,
    /// Not enough bytes were available to complete decoding.
    Underrun,
    /// A CBOR text string contains invalid UTF-8 data.
    Utf8Error,
    /// The byte sequence cannot be decoded as CBOR data.
    Undecodable,
    /// Improper nesting of types inside an indefinite text or byte string.
    BadSubString,
    /// CBOR elements were terminated by a BREAK symbol.
    Break,
    /// CBOR element was marked as indefinite-length.
    Indefinite,
    /// A Map didn't have an even number of members.
    MapPairError,
    /// An unknown Simple Value was encountered.
    UnknownSimple(u8),
}

/// Binary CBOR encoding.
pub trait Decode {
    /// Decode data to a [`CborType`] list.
    fn decode(&self) -> Result<Vec<CborType>, DecodeError>;
}

/// Symbolic CBOR decoding.
pub trait DecodeSymbolic {
    /// Decode data to a low-level [`format::Element`] list.
    fn decode_symbolic(&self) -> Result<Vec<format::Element>, DecodeError>;
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
