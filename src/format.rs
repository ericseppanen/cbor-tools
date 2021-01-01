use crate::{
    Array, ByteString, CborType, Decode, DecodeSymbolic, Encode, EncodeSymbolic, Float, Indefinite,
    Integer, Map, Tagged, TextString,
};
use std::convert::TryInto;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Major {
    Uint = 0,
    Nint = 1,
    Bstr = 2,
    Tstr = 3,
    Array = 4,
    Map = 5,
    Tag = 6,
    Misc = 7,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct AdnInfo(u8);

impl AdnInfo {
    // Additional Information 24 to 27 are used to communicate
    // 1, 2, 4, or 8 bytes following.
    pub const MORE1: AdnInfo = AdnInfo(24);
    pub const MORE2: AdnInfo = AdnInfo(25);
    pub const MORE4: AdnInfo = AdnInfo(26);
    pub const MORE8: AdnInfo = AdnInfo(27);

    // Indefinite-length byte- or text- strings begin with this
    pub const INDEFINITE: AdnInfo = AdnInfo(31);

    // In major type 7, a number of values are used for special purposes.
    pub const FALSE: AdnInfo = AdnInfo(20);
    pub const TRUE: AdnInfo = AdnInfo(21);
    pub const NULL: AdnInfo = AdnInfo(22);
    pub const UNDEFINED: AdnInfo = AdnInfo(23);
    pub const FLOAT16: AdnInfo = AdnInfo(25);
    pub const FLOAT32: AdnInfo = AdnInfo(26);
    pub const FLOAT64: AdnInfo = AdnInfo(27);
    pub const BREAK: AdnInfo = AdnInfo(31);
}

// A struct used to signal that the element carries no further bytes
struct Nada;

impl From<Nada> for Vec<u8> {
    fn from(_: Nada) -> Self {
        Vec::new()
    }
}

// Used as a function argument to specify definite-length or
// indefinite-length encoding.
#[derive(PartialEq)]
struct UseDefLen(bool);
const AS_DEF: UseDefLen = UseDefLen(true);
const AS_INDEF: UseDefLen = UseDefLen(false);

#[derive(Clone, Debug, PartialEq)]
pub struct Element {
    major: Major,
    adn_info: AdnInfo,
    bytes: Vec<u8>,
}

impl Element {
    fn new<T>(major: Major, adn_info: AdnInfo, bytes: T) -> Element
    where
        T: Into<Vec<u8>>,
    {
        Element {
            major,
            adn_info,
            bytes: bytes.into(),
        }
    }

    fn add_bytes(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes);
    }
}

impl Decode for Vec<Element> {
    fn decode(&self) -> Vec<CborType> {
        todo!()
    }
}

impl DecodeSymbolic for Vec<u8> {
    fn decode_symbolic(&self) -> Vec<Element> {
        todo!()
    }
}

impl Decode for Vec<u8> {
    fn decode(&self) -> Vec<CborType> {
        todo!()
    }
}

impl EncodeSymbolic for CborType {
    fn encode_symbolic(&self) -> Vec<Element> {
        match self {
            CborType::Null => {
                let element = Element::new(Major::Misc, AdnInfo::NULL, Nada);
                vec![element]
            }
            CborType::Undefined => {
                let element = Element::new(Major::Misc, AdnInfo::UNDEFINED, Nada);
                vec![element]
            }
            CborType::Bool(val) => {
                let adn_info = if *val { AdnInfo::TRUE } else { AdnInfo::FALSE };
                let element = Element::new(Major::Misc, adn_info, Nada);
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
        Integer::U5(n) => Element::new(Major::Uint, AdnInfo(*n), Nada),
        Integer::U8(n) => Element::new(Major::Uint, AdnInfo::MORE1, vec![n]),
        Integer::U16(n) => Element::new(Major::Uint, AdnInfo::MORE2, n.to_be_bytes()),
        Integer::U32(n) => Element::new(Major::Uint, AdnInfo::MORE4, n.to_be_bytes()),
        Integer::U64(n) => Element::new(Major::Uint, AdnInfo::MORE8, n.to_be_bytes()),
        Integer::N5(n) => Element::new(Major::Nint, AdnInfo(*n), Nada),
        Integer::N8(n) => Element::new(Major::Nint, AdnInfo::MORE1, vec![n]),
        Integer::N16(n) => Element::new(Major::Nint, AdnInfo::MORE2, n.to_be_bytes()),
        Integer::N32(n) => Element::new(Major::Nint, AdnInfo::MORE4, n.to_be_bytes()),
        Integer::N64(n) => Element::new(Major::Nint, AdnInfo::MORE8, n.to_be_bytes()),
    };
    vec![element]
}

// Encode a text- or byte-string into an Element.
fn encode_bytes(major: Major, v: &[u8]) -> Element {
    let mut element = encode_length(major, v.len());
    element.add_bytes(v);
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
        Element::new(major, AdnInfo(len as u8), Nada)
    } else if len < 0x100 {
        // 1 byte needed to express length.
        let mut buf = Vec::new();
        buf.push(len as u8);
        Element::new(major, AdnInfo::MORE1, buf)
    } else if len < 0x10000 {
        // 2 bytes needed to express length.
        let mut buf = Vec::new();
        buf.extend(&(len as u16).to_be_bytes());
        Element::new(major, AdnInfo::MORE2, buf)
    } else if len < 0x100000000 {
        // 4 bytes needed to express length.
        let mut buf = Vec::new();
        buf.extend(&(len as u32).to_be_bytes());
        Element::new(major, AdnInfo::MORE4, buf)
    } else {
        // 8 bytes needed to express length.
        let mut buf = Vec::new();
        buf.extend(&(len as u64).to_be_bytes());
        Element::new(major, AdnInfo::MORE8, buf)
    }
}

/// Encode a byte string.
fn encode_bytestring(bstr: &ByteString) -> Vec<Element> {
    let element = encode_bytes(Major::Bstr, &bstr.0);
    vec![element]
}

fn encode_indef_bytestring(list: &Vec<ByteString>) -> Vec<Element> {
    let mut elements = Vec::with_capacity(1 + list.len());
    elements.push(Element::new(Major::Bstr, AdnInfo::INDEFINITE, Nada));
    for bstr in list {
        elements.push(encode_bytes(Major::Bstr, &bstr.0));
    }
    elements.push(Element::new(Major::Misc, AdnInfo::BREAK, Nada));
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
    elements.push(Element::new(Major::Tstr, AdnInfo::INDEFINITE, Nada));
    for text in list {
        let bytes = text.0.as_bytes();
        elements.push(encode_bytes(Major::Tstr, bytes));
    }
    elements.push(Element::new(Major::Misc, AdnInfo::BREAK, Nada));
    elements
}

fn encode_array(a: &Array, definite: UseDefLen) -> Vec<Element> {
    let list = &a.0;
    let mut elements = Vec::with_capacity(1 + list.len());
    if definite == AS_INDEF {
        elements.push(Element::new(Major::Array, AdnInfo::INDEFINITE, Nada));
    } else {
        elements.push(encode_length(Major::Array, list.len()));
    }
    for item in list {
        elements.extend(item.encode_symbolic());
    }
    if definite == AS_INDEF {
        elements.push(Element::new(Major::Misc, AdnInfo::BREAK, Nada));
    }
    elements
}

fn encode_map(map: &Map, definite: UseDefLen) -> Vec<Element> {
    let kv_list = &map.0;
    let mut elements = Vec::with_capacity(1 + kv_list.len());
    if definite == AS_INDEF {
        elements.push(Element::new(Major::Map, AdnInfo::INDEFINITE, Nada));
    } else {
        elements.push(encode_length(Major::Map, kv_list.len()));
    }
    for (k, v) in kv_list {
        elements.extend(k.encode_symbolic());
        elements.extend(v.encode_symbolic());
    }
    if definite == AS_INDEF {
        elements.push(Element::new(Major::Misc, AdnInfo::BREAK, Nada));
    }
    elements
}

fn encode_float(f: &Float) -> Vec<Element> {
    let element = match f {
        Float::F16(n) => Element::new(Major::Misc, AdnInfo::FLOAT16, n.to_be_bytes()),
        Float::F32(n) => Element::new(Major::Misc, AdnInfo::FLOAT32, n.to_be_bytes()),
        Float::F64(n) => Element::new(Major::Misc, AdnInfo::FLOAT64, n.to_be_bytes()),
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
        buf.extend(&self.bytes);
        buf
    }
}
