use crate::{
    Array, ByteString, CborType, Decode, DecodeError, DecodeSymbolic, Encode, EncodeSymbolic,
    Float, Indefinite, Integer, Map, Tag, Tagged, TextString, ZeroTo23,
};
use half::f16;
use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use std::convert::TryInto;

/// The major number in a CBOR encoding
///
/// The major number is 3 bits long, and identifies the basic
/// type of a CBOR-encoded value.
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, TryFromPrimitive)]
pub enum Major {
    /// An unsigned integer
    Uint = 0,
    /// A negative integer
    Nint = 1,
    /// A byte string
    Bstr = 2,
    /// A text string
    Tstr = 3,
    /// An array
    Array = 4,
    /// A map
    Map = 5,
    /// A tagged value
    Tag = 6,
    /// Miscellaneous types (floats, bool, null, etc)
    Misc = 7,
}

/// The "additional information" field
///
/// This is a 5-bit field used to communicate some more
/// detail about the value; it's commonly used to communicate
/// simple values (True, False, Null) or specify how many bytes
/// are to follow.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AdnInfo(u8);

#[allow(missing_docs)]
impl AdnInfo {
    /// 1 byte to follow.
    pub const MORE1: AdnInfo = AdnInfo(24);
    /// 2 bytes to follow.
    pub const MORE2: AdnInfo = AdnInfo(25);
    /// 4 bytes to follow.
    pub const MORE4: AdnInfo = AdnInfo(26);
    /// 8 bytes to follow.
    pub const MORE8: AdnInfo = AdnInfo(27);

    /// Indefinite-length encoding is used.
    pub const INDEFINITE: AdnInfo = AdnInfo(31);

    // In major type 7, a number of values are used for special purposes.
    pub const FALSE: AdnInfo = AdnInfo(20);
    pub const TRUE: AdnInfo = AdnInfo(21);
    pub const NULL: AdnInfo = AdnInfo(22);
    pub const UNDEFINED: AdnInfo = AdnInfo(23);
    pub const FLOAT16: AdnInfo = AdnInfo(25);
    pub const FLOAT32: AdnInfo = AdnInfo(26);
    pub const FLOAT64: AdnInfo = AdnInfo(27);
    /// Terminate an indefinite-length encoding.
    pub const BREAK: AdnInfo = AdnInfo(31);
}

impl From<u8> for AdnInfo {
    fn from(n: u8) -> Self {
        if n >= 24 {
            panic!("can't create AdnInfo for n={}", n);
        }
        AdnInfo(n)
    }
}

/// A zero-to-8 byte immediate value
///
/// Many CBOR encodings use an integer value that can range from
/// zero to 8 bytes in length.  This is often used to encode the
/// length of some data, or may be the payload value itself (in
/// the case of integers or floats).
///
/// The contained array is in big-endian format.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ImmediateValue {
    /// No immediate value.
    Empty,
    /// One-byte immediate value.
    Bytes1([u8; 1]),
    /// Two-byte immediate value.
    Bytes2([u8; 2]),
    /// Four-byte immediate value.
    Bytes4([u8; 4]),
    /// Eight-byte immediate value.
    Bytes8([u8; 8]),
}

impl<'a> IntoIterator for &'a ImmediateValue {
    type Item = &'a u8;
    type IntoIter = std::slice::Iter<'a, u8>;

    fn into_iter(self) -> Self::IntoIter {
        // We need something so the Empty case returns the same type as the others.
        const IMM_DUMMY: [u8; 0] = [];

        match self {
            ImmediateValue::Empty => IMM_DUMMY.iter(),
            ImmediateValue::Bytes1(b) => b.iter(),
            ImmediateValue::Bytes2(b) => b.iter(),
            ImmediateValue::Bytes4(b) => b.iter(),
            ImmediateValue::Bytes8(b) => b.iter(),
        }
    }
}

impl From<u8> for ImmediateValue {
    fn from(n: u8) -> Self {
        let bytes = n.to_be_bytes();
        ImmediateValue::Bytes1(bytes)
    }
}

impl From<u16> for ImmediateValue {
    fn from(n: u16) -> Self {
        let bytes = n.to_be_bytes();
        ImmediateValue::Bytes2(bytes)
    }
}

impl From<u32> for ImmediateValue {
    fn from(n: u32) -> Self {
        let bytes = n.to_be_bytes();
        ImmediateValue::Bytes4(bytes)
    }
}

impl From<u64> for ImmediateValue {
    fn from(n: u64) -> Self {
        let bytes = n.to_be_bytes();
        ImmediateValue::Bytes8(bytes)
    }
}

impl From<f16> for ImmediateValue {
    fn from(n: f16) -> Self {
        let bytes = n.to_be_bytes();
        ImmediateValue::Bytes2(bytes)
    }
}

impl From<f32> for ImmediateValue {
    fn from(n: f32) -> Self {
        let bytes = n.to_be_bytes();
        ImmediateValue::Bytes4(bytes)
    }
}

impl From<f64> for ImmediateValue {
    fn from(n: f64) -> Self {
        let bytes = n.to_be_bytes();
        ImmediateValue::Bytes8(bytes)
    }
}

impl From<ImmediateValue> for u64 {
    fn from(imm: ImmediateValue) -> u64 {
        match imm {
            ImmediateValue::Bytes1(b) => u8::from_be_bytes(b).into(),
            ImmediateValue::Bytes2(b) => u16::from_be_bytes(b).into(),
            ImmediateValue::Bytes4(b) => u32::from_be_bytes(b).into(),
            ImmediateValue::Bytes8(b) => u64::from_be_bytes(b),
            _ => panic!("can't convert {:?} to u64", imm),
        }
    }
}

// Used as a function argument to specify definite-length or
// indefinite-length encoding.
#[derive(PartialEq)]
struct UseDefLen(bool);
const AS_DEF: UseDefLen = UseDefLen(true);
const AS_INDEF: UseDefLen = UseDefLen(false);

/// A unit of CBOR-encoded data
///
/// Each `Element` contains:
/// - A major number, indicating the type of data.
/// - An "additional information" field.
/// - A Zero-to-eight byte immediate value (containing a numeric value or length).
/// - An optional sequence of payload bytes.
///
/// See RFC 7049 for details.
///
/// Some [`CborType`] values will require multiple [`Element`]s to encode.
#[derive(Clone, Debug, PartialEq)]
pub struct Element {
    // The major number.
    major: Major,
    // The "additional information" field.
    adn_info: AdnInfo,
    imm: ImmediateValue,
    bytes: Vec<u8>,
}

impl Element {
    /// Create a new Element, with no payload buffer.
    pub fn new(major: Major, adn_info: AdnInfo, imm: ImmediateValue) -> Element {
        Element {
            major,
            adn_info,
            imm,
            bytes: Vec::new(),
        }
    }

    /// Create a new Element, with no payload buffer or immediate data.
    pub fn simple(major: Major, adn_info: AdnInfo) -> Element {
        Element {
            major,
            adn_info,
            imm: ImmediateValue::Empty,
            bytes: Vec::new(),
        }
    }

    /// Add a payload buffer to this Element.
    pub fn set_bytes(&mut self, bytes: &[u8]) {
        if !self.bytes.is_empty() {
            panic!("set_bytes on nonempty Element");
        }
        self.bytes.extend(bytes);
    }

    // This is part of the decoding phase.
    // We will decode the major and adn_info only.
    fn from_byte(byte: u8) -> Element {
        // This can't actually fail, since all 8 values are valid variants.
        let major = Major::try_from(byte >> 5).unwrap();
        let adn_info = AdnInfo(byte & 0x1F);
        let imm = ImmediateValue::Empty;
        Element {
            major,
            adn_info,
            imm,
            bytes: Vec::new(),
        }
    }
}

impl Decode for Vec<Element> {
    fn decode(&self) -> Result<Vec<CborType>, DecodeError> {
        let mut result = Vec::new();
        let mut input = self.iter();
        loop {
            let decoded = decode_one(&mut input);
            match decoded {
                Ok(val) => result.push(val),
                Err(DecodeError::Underrun) => {
                    // FIXME: is this right?
                    break;
                }
                Err(e) => return Err(e),
            }
        }
        Ok(result)
    }
}

// Decode one CborType from the input iterator.
//
// If the input iterator returns None, this will return Err(Underrun).
// If the input is a BREAK, this will return Err(Break).
// All other well-formed inputs will return a single CborType.
fn decode_one(input: &mut std::slice::Iter<'_, Element>) -> Result<CborType, DecodeError> {
    let decoded = match input.next() {
        None => {
            return Err(DecodeError::Underrun);
        }
        Some(element) => match element.major {
            Major::Uint => decode_uint(element),
            Major::Nint => decode_nint(element),
            Major::Bstr => {
                if element.adn_info == AdnInfo::INDEFINITE {
                    // Indefinite-length byte string
                    // FIXME: is it possible for the element to have improper fields?
                    // imm? bytes? either it's guaranteed to be well-formed here, or
                    // we need extra checks?
                    decode_bstr_indef(input)
                } else {
                    decode_bstr(element)
                }
            }
            Major::Tstr => {
                if element.adn_info == AdnInfo::INDEFINITE {
                    // Indefinite-length text string
                    decode_tstr_indef(input)
                } else {
                    decode_tstr(element)
                }
            }
            Major::Array => decode_array(element, input),
            Major::Map => decode_map(element, input),
            Major::Tag => decode_tag(element, input),
            Major::Misc => decode_misc(element),
        },
    }?;
    Ok(decoded)
}

fn decode_misc(element: &Element) -> Result<CborType, DecodeError> {
    let decoded = match element.adn_info {
        AdnInfo::FALSE => CborType::Bool(false),
        AdnInfo::TRUE => CborType::Bool(true),
        AdnInfo::NULL => CborType::Null,
        AdnInfo::UNDEFINED => CborType::Undefined,
        AdnInfo::BREAK => return Err(DecodeError::Break),
        _ => todo!(),
    };
    Ok(decoded)
}

fn decode_uint(element: &Element) -> Result<CborType, DecodeError> {
    let decoded = match (element.adn_info, element.imm) {
        (AdnInfo(n), ImmediateValue::Empty) if n < 24 => Integer::U5(ZeroTo23::new(n)),
        (AdnInfo::MORE1, ImmediateValue::Bytes1(b)) => Integer::U8(b[0]),
        (AdnInfo::MORE2, ImmediateValue::Bytes2(b)) => Integer::U16(u16::from_be_bytes(b)),
        (AdnInfo::MORE4, ImmediateValue::Bytes4(b)) => Integer::U32(u32::from_be_bytes(b)),
        (AdnInfo::MORE8, ImmediateValue::Bytes8(b)) => Integer::U64(u64::from_be_bytes(b)),
        _ => panic!("nonsensical uint element"),
    };
    Ok(decoded.into())
}

fn decode_nint(element: &Element) -> Result<CborType, DecodeError> {
    let decoded = match (element.adn_info, element.imm) {
        (AdnInfo(n), ImmediateValue::Empty) if n < 24 => Integer::N5(ZeroTo23::new(n)),
        (AdnInfo::MORE1, ImmediateValue::Bytes1(b)) => Integer::N8(b[0]),
        (AdnInfo::MORE2, ImmediateValue::Bytes2(b)) => Integer::N16(u16::from_be_bytes(b)),
        (AdnInfo::MORE4, ImmediateValue::Bytes4(b)) => Integer::N32(u32::from_be_bytes(b)),
        (AdnInfo::MORE8, ImmediateValue::Bytes8(b)) => Integer::N64(u64::from_be_bytes(b)),
        _ => panic!("nonsensical uint element"),
    };
    Ok(decoded.into())
}

fn decode_bstr(element: &Element) -> Result<CborType, DecodeError> {
    Ok(CborType::ByteString(ByteString(element.bytes.clone())))
}

fn decode_tstr(element: &Element) -> Result<CborType, DecodeError> {
    let text = String::from_utf8(element.bytes.clone()).map_err(|_| DecodeError::Utf8Error)?;
    Ok(CborType::TextString(TextString(text)))
}

fn decode_bstr_indef(input: &mut std::slice::Iter<'_, Element>) -> Result<CborType, DecodeError> {
    // Consume a run of elements containing bstrs, terminated by a BREAK symbol.
    let mut result: Vec<ByteString> = Vec::new();
    loop {
        match decode_one(input) {
            Ok(CborType::ByteString(b)) => result.push(b),
            Ok(_) => return Err(DecodeError::BadSubString),
            Err(DecodeError::Break) => break,
            Err(e) => return Err(e),
        }
    }
    Ok(CborType::Indefinite(Indefinite::ByteString(result)))
}

fn decode_tstr_indef(input: &mut std::slice::Iter<'_, Element>) -> Result<CborType, DecodeError> {
    // Consume a run of elements containing tstrs, terminated by a BREAK symbol.
    let mut result: Vec<TextString> = Vec::new();
    loop {
        match decode_one(input) {
            Ok(CborType::TextString(t)) => result.push(t),
            Ok(_) => return Err(DecodeError::BadSubString),
            Err(DecodeError::Break) => break,
            Err(e) => return Err(e),
        }
    }
    Ok(CborType::Indefinite(Indefinite::TextString(result)))
}

// A counter for keeping track of how many more elements we want.
enum RunLength {
    Indefinite,
    Definite(usize),
}

impl RunLength {
    // Given an element, create a RunLength counter.
    fn get(element: &Element) -> Result<Self, DecodeError> {
        match get_length(&element) {
            Ok(len) => {
                // FIXME: if this is a map, double the length?
                // Or let the caller handle it?
                Ok(RunLength::Definite(len))
            }
            Err(DecodeError::Indefinite) => Ok(RunLength::Indefinite),
            Err(e) => Err(e),
        }
    }

    fn is_zero(&self) -> bool {
        match self {
            RunLength::Indefinite => false,
            RunLength::Definite(len) => *len == 0,
        }
    }

    fn decrement(&mut self) {
        match self {
            RunLength::Indefinite => {}
            RunLength::Definite(ref mut len) => {
                if *len == 0 {
                    panic!("RunLength underflow");
                }
                *len -= 1;
            }
        }
    }
}

fn decode_array(
    element: &Element,
    input: &mut std::slice::Iter<'_, Element>,
) -> Result<CborType, DecodeError> {
    let mut rlen = RunLength::get(element)?;
    let is_indef = matches!(rlen, RunLength::Indefinite);
    let mut result: Vec<CborType> = Vec::new();
    while !rlen.is_zero() {
        // Recursively decode one CborType, by traversing one or more Elements.
        let val = decode_one(input);
        if is_indef && matches!(val, Err(DecodeError::Break)) {
            // This is an indefinite-length array, properly terminated.
            break;
        }
        // Keep any Ok result; return any remaining error.
        result.push(val?);
        // Decrement the array-length counter.
        rlen.decrement();
    }

    // rlen is now zero; return the result
    if is_indef {
        Ok(CborType::Indefinite(Indefinite::Array(Array::from(result))))
    } else {
        Ok(CborType::from(result))
    }
}

fn decode_map(
    element: &Element,
    input: &mut std::slice::Iter<'_, Element>,
) -> Result<CborType, DecodeError> {
    let rlen = RunLength::get(element)?;

    // Maps consume 2n items, so double the length.
    let mut rlen = match rlen {
        RunLength::Indefinite => rlen,
        RunLength::Definite(n) => RunLength::Definite(2 * n),
    };

    let is_indef = matches!(rlen, RunLength::Indefinite);
    let mut result: Vec<CborType> = Vec::new();
    while !rlen.is_zero() {
        // Recursively decode one CborType, by traversing one or more Elements.
        let val = decode_one(input);
        if is_indef && matches!(val, Err(DecodeError::Break)) {
            // This is an indefinite-length map, properly terminated.
            break;
        }
        // Keep any Ok result; return any remaining error.
        result.push(val?);
        // Decrement the map-length counter.
        rlen.decrement();
    }
    // rlen is now zero; return the result

    // convert the result to a Vec of (key, value) pairs.
    let mut results = result.drain(..);
    let mut map_pairs = Vec::new();
    loop {
        let kv_pair = (results.next(), results.next());
        match kv_pair {
            (Some(k), Some(v)) => map_pairs.push((k, v)),
            (None, None) => break,
            _ => return Err(DecodeError::MapPairError),
        }
    }

    if is_indef {
        Ok(CborType::Indefinite(Indefinite::Map(Map::from(map_pairs))))
    } else {
        Ok(CborType::from(map_pairs))
    }
}

fn decode_tag(
    element: &Element,
    input: &mut std::slice::Iter<'_, Element>,
) -> Result<CborType, DecodeError> {
    let child_value = decode_one(input)?;
    // FIXME: are there ways the "tag" element might be malformed?
    // FIXME: need a better approach to the usize/u64 adaptations.
    let tag = get_length(element)? as u64;

    Ok(CborType::Tagged(Tagged {
        tag: Tag(tag),
        child: Box::new(child_value),
    }))
}

// In the future this could complete the convertion to [u8; N]
// using const generics.
fn try_split(slice: &[u8], index: usize) -> Result<(&[u8], &[u8]), DecodeError> {
    if slice.len() < index {
        Err(DecodeError::Underrun)
    } else {
        Ok(slice.split_at(index))
    }
}

fn decode_imm(element: &mut Element, buf: &mut &[u8]) -> Result<(), DecodeError> {
    match element.imm {
        ImmediateValue::Empty => (),
        _ => {
            panic!("decode_imm called on element that already has immediate data");
        }
    }

    match element.adn_info {
        AdnInfo::MORE1 => {
            element.imm = ImmediateValue::from(buf[0]);
            *buf = &buf[1..];
        }
        AdnInfo::MORE2 => {
            let (head, tail) = try_split(*buf, 2)?;
            let imm: [u8; 2] = head.as_ref().try_into().unwrap();
            element.imm = ImmediateValue::Bytes2(imm);
            *buf = tail;
        }
        AdnInfo::MORE4 => {
            let (head, tail) = try_split(*buf, 4)?;
            let imm: [u8; 4] = head.as_ref().try_into().unwrap();
            element.imm = ImmediateValue::Bytes4(imm);
            *buf = tail;
        }
        AdnInfo::MORE8 => {
            let (head, tail) = try_split(*buf, 8)?;
            let imm: [u8; 8] = head.as_ref().try_into().unwrap();
            element.imm = ImmediateValue::Bytes8(imm);
            *buf = tail;
        }
        _ => {
            // FIXME: Not entirely sure what I want to do here.
            // Right now, adn_info is a strange value I'll just punt
            // to the next level of decode.
        }
    }
    Ok(())
}

fn get_length(element: &Element) -> Result<usize, DecodeError> {
    match element.adn_info {
        AdnInfo(n) if n < 24 => Ok(n as usize),
        AdnInfo::MORE1 | AdnInfo::MORE2 | AdnInfo::MORE4 | AdnInfo::MORE8 => {
            let length: u64 = element.imm.into();
            let length: usize = length.try_into().unwrap();
            Ok(length)
        }
        AdnInfo::INDEFINITE => Err(DecodeError::Indefinite),
        _ => Err(DecodeError::Undecodable),
    }
}

impl DecodeSymbolic for [u8] {
    fn decode_symbolic(&self) -> Result<Vec<Element>, DecodeError> {
        let mut remaining = &self[..];
        let mut result = Vec::new();
        loop {
            if remaining.is_empty() {
                break;
            }
            // Convert first byte's fields into an Element
            let mut element = Element::from_byte(remaining[0]);
            remaining = &remaining[1..];
            // take 0-8 bytes based on adn_info
            decode_imm(&mut element, &mut remaining)?;
            // Examine the Element; based on its values, take some
            // bytes into its 'bytes' field.
            match element.major {
                Major::Uint | Major::Nint => {
                    // Nothing further needed here.
                }
                Major::Bstr | Major::Tstr => {
                    match get_length(&element) {
                        Ok(length) => {
                            // This is a definite-length encoding;
                            // take that many more bytes as payload.
                            let (head, tail) = try_split(&mut remaining, length)?;
                            element.bytes = head.into();
                            remaining = tail;
                        }
                        Err(DecodeError::Indefinite) => {}
                        Err(e) => return Err(e),
                    }
                }
                Major::Array | Major::Map => {
                    // Nothing further needed here.
                    // Each array or map member is its own element.
                }
                Major::Tag => {
                    // Nothing further needed here.
                }
                Major::Misc => {
                    // No further action needed for bool/null/undefined
                    // FIXME: handle floats
                    // FIXME: handle other weird/invalid values?
                }
            }
            result.push(element);
        }

        Ok(result)
    }
}

impl Decode for [u8] {
    fn decode(&self) -> Result<Vec<CborType>, DecodeError> {
        self.decode_symbolic()?.decode()
    }
}

impl EncodeSymbolic for CborType {
    fn encode_symbolic(&self) -> Vec<Element> {
        match self {
            CborType::Null => {
                let element = Element::new(Major::Misc, AdnInfo::NULL, ImmediateValue::Empty);
                vec![element]
            }
            CborType::Undefined => {
                let element = Element::new(Major::Misc, AdnInfo::UNDEFINED, ImmediateValue::Empty);
                vec![element]
            }
            CborType::Bool(val) => {
                let adn_info = if *val { AdnInfo::TRUE } else { AdnInfo::FALSE };
                let element = Element::new(Major::Misc, adn_info, ImmediateValue::Empty);
                vec![element]
            }
            CborType::Integer(x) => encode_integer(x),
            CborType::ByteString(x) => encode_bytestring(x),
            CborType::TextString(x) => encode_textstring(x),
            CborType::Array(x) => encode_array(x, AS_DEF),
            CborType::Map(m) => encode_map(m, AS_DEF),
            CborType::Indefinite(x) => encode_indefinite(x),
            CborType::Tagged(x) => encode_tagged(x),
            CborType::Float(x) => encode_float(x),
        }
    }
}

fn encode_indefinite(ind: &Indefinite) -> Vec<Element> {
    match ind {
        Indefinite::ByteString(x) => encode_indef_bytestring(x),
        Indefinite::TextString(x) => encode_indef_textstring(x),
        Indefinite::Array(x) => encode_array(x, AS_INDEF),
        Indefinite::Map(x) => encode_map(x, AS_INDEF),
    }
}

/// Encode an integer.
///
/// This does not attempt to canonicalize the integer size; a small number stored
/// as an Integer::U32, for example, will be encoded as 5 bytes.
fn encode_integer(x: &Integer) -> Vec<Element> {
    let element = match *x {
        Integer::U5(n) => Element::new(Major::Uint, AdnInfo(*n), ImmediateValue::Empty),
        Integer::U8(n) => Element::new(Major::Uint, AdnInfo::MORE1, n.into()),
        Integer::U16(n) => Element::new(Major::Uint, AdnInfo::MORE2, n.into()),
        Integer::U32(n) => Element::new(Major::Uint, AdnInfo::MORE4, n.into()),
        Integer::U64(n) => Element::new(Major::Uint, AdnInfo::MORE8, n.into()),
        Integer::N5(n) => Element::new(Major::Nint, AdnInfo(*n), ImmediateValue::Empty),
        Integer::N8(n) => Element::new(Major::Nint, AdnInfo::MORE1, n.into()),
        Integer::N16(n) => Element::new(Major::Nint, AdnInfo::MORE2, n.into()),
        Integer::N32(n) => Element::new(Major::Nint, AdnInfo::MORE4, n.into()),
        Integer::N64(n) => Element::new(Major::Nint, AdnInfo::MORE8, n.into()),
    };
    vec![element]
}

// Encode a text- or byte-string into an Element.
fn encode_bytes(major: Major, v: &[u8]) -> Element {
    let mut element = encode_length(major, v.len());
    element.set_bytes(v);
    element
}

// Adapter for places that have a usize in hand.
// Hopefully this is optimized away, as usize->u64 should
// always succeed.
#[inline]
fn encode_length(major: Major, len: usize) -> Element {
    let len: u64 = len.try_into().expect("usize to u64");
    encode_immediate(major, len)
}

// Helper function for length values
//
// It returns an Element with no payload.
// This is incomplete for definite-length byte/text-strings,
// which need to have the string bytes appended.
// It is correct for arrays or maps.
fn encode_immediate(major: Major, len: u64) -> Element {
    if len < 24 {
        Element::new(major, AdnInfo(len as u8), ImmediateValue::Empty)
    } else if len < 0x100 {
        // 1 byte needed to express length.
        let len = len as u8;
        Element::new(major, AdnInfo::MORE1, len.into())
    } else if len < 0x10000 {
        // 2 bytes needed to express length.
        let len = len as u16;
        Element::new(major, AdnInfo::MORE2, len.into())
    } else if len < 0x100000000 {
        // 4 bytes needed to express length.
        let len = len as u32;
        Element::new(major, AdnInfo::MORE4, len.into())
    } else {
        // 8 bytes needed to express length.
        let len = len as u64;
        Element::new(major, AdnInfo::MORE8, len.into())
    }
}

/// Encode a byte string.
fn encode_bytestring(bstr: &ByteString) -> Vec<Element> {
    let element = encode_bytes(Major::Bstr, &bstr.0);
    vec![element]
}

fn encode_indef_bytestring(list: &Vec<ByteString>) -> Vec<Element> {
    let mut elements = Vec::with_capacity(1 + list.len());
    elements.push(Element::simple(Major::Bstr, AdnInfo::INDEFINITE));
    for bstr in list {
        elements.push(encode_bytes(Major::Bstr, &bstr.0));
    }
    elements.push(Element::simple(Major::Misc, AdnInfo::BREAK));
    elements
}

/// Encode a text string.
fn encode_textstring(text: &TextString) -> Vec<Element> {
    let bytes = text.0.as_bytes();
    let element = encode_bytes(Major::Tstr, bytes);
    vec![element]
}

fn encode_indef_textstring(list: &Vec<TextString>) -> Vec<Element> {
    let mut elements = Vec::with_capacity(1 + list.len());
    elements.push(Element::simple(Major::Tstr, AdnInfo::INDEFINITE));
    for text in list {
        let bytes = text.0.as_bytes();
        elements.push(encode_bytes(Major::Tstr, bytes));
    }
    elements.push(Element::simple(Major::Misc, AdnInfo::BREAK));
    elements
}

fn encode_array(a: &Array, definite: UseDefLen) -> Vec<Element> {
    let list = &a.0;
    let mut elements = Vec::with_capacity(1 + list.len());
    if definite == AS_INDEF {
        elements.push(Element::simple(Major::Array, AdnInfo::INDEFINITE));
    } else {
        elements.push(encode_length(Major::Array, list.len()));
    }
    for item in list {
        elements.extend(item.encode_symbolic());
    }
    if definite == AS_INDEF {
        elements.push(Element::simple(Major::Misc, AdnInfo::BREAK));
    }
    elements
}

fn encode_map(map: &Map, definite: UseDefLen) -> Vec<Element> {
    let kv_list = &map.0;
    let mut elements = Vec::with_capacity(1 + kv_list.len());
    if definite == AS_INDEF {
        elements.push(Element::simple(Major::Map, AdnInfo::INDEFINITE));
    } else {
        elements.push(encode_length(Major::Map, kv_list.len()));
    }
    for (k, v) in kv_list {
        elements.extend(k.encode_symbolic());
        elements.extend(v.encode_symbolic());
    }
    if definite == AS_INDEF {
        elements.push(Element::simple(Major::Misc, AdnInfo::BREAK));
    }
    elements
}

fn encode_float(f: &Float) -> Vec<Element> {
    let element = match *f {
        Float::F16(n) => Element::new(Major::Misc, AdnInfo::FLOAT16, n.into()),
        Float::F32(n) => Element::new(Major::Misc, AdnInfo::FLOAT32, n.into()),
        Float::F64(n) => Element::new(Major::Misc, AdnInfo::FLOAT64, n.into()),
    };
    vec![element]
}

fn encode_tagged(x: &Tagged) -> Vec<Element> {
    let tag = encode_immediate(Major::Tag, x.tag.0);
    let mut v = Vec::<Element>::new();
    v.push(tag);
    v.extend(x.child.encode_symbolic());
    v
}

impl Encode for Vec<Element> {
    fn encode(&self) -> Vec<u8> {
        self.iter().map(Encode::encode).flatten().collect()
    }
}

impl Encode for Element {
    fn encode(&self) -> Vec<u8> {
        let major = self.major as u8;
        if major > 7 {
            panic!("major out of range");
        }
        if self.adn_info.0 > 31 {
            panic!("additional-info out of range");
        }
        let mut buf = Vec::with_capacity(1 + self.bytes.len());
        buf.push(major << 5 | self.adn_info.0);
        buf.extend(&self.imm);
        buf.extend(&self.bytes);
        buf
    }
}
