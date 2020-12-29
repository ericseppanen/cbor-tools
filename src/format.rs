use crate::{CborType, Decode, DecodeSymbolic, Encode, EncodeSymbolic, Integer};
use byteorder::{ByteOrder, NetworkEndian};

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

#[derive(Clone, Debug, PartialEq)]
pub struct Element {
    pub major: Major,
    pub adn_info: AdnInfo,
    pub bytes: Vec<u8>,
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
            CborType::ByteString(_) => todo!(),
            CborType::TextString(_) => todo!(),
            CborType::Array(_) => todo!(),
            CborType::Map(_) => todo!(),
            CborType::Tagged(_) => todo!(),
            CborType::Float(_) => todo!(),
        }
    }
}

// This is the canonical encoding.
fn encode_integer(x: &Integer) -> Vec<Element> {
    let element = match *x {
        Integer::U5(n) => Element::new(Major::Uint, AdnInfo(*n), Nada),
        Integer::U8(n) => Element::new(Major::Uint, AdnInfo::MORE1, vec![n]),
        Integer::U16(n) => {
            let mut buf = [0u8; 2];
            NetworkEndian::write_u16(&mut buf, n);
            Element::new(Major::Uint, AdnInfo::MORE2, buf)
        }
        Integer::U32(n) => {
            let mut buf = [0u8; 4];
            NetworkEndian::write_u32(&mut buf, n);
            Element::new(Major::Uint, AdnInfo::MORE4, buf)
        }
        Integer::U64(n) => {
            let mut buf = [0u8; 8];
            NetworkEndian::write_u64(&mut buf, n);
            Element::new(Major::Uint, AdnInfo::MORE8, buf)
        }
        Integer::N5(n) => Element::new(Major::Nint, AdnInfo(*n), Nada),
        Integer::N8(n) => Element::new(Major::Nint, AdnInfo::MORE1, vec![n]),
        Integer::N16(n) => {
            let mut buf = [0u8; 2];
            NetworkEndian::write_u16(&mut buf, n);
            Element::new(Major::Nint, AdnInfo::MORE2, buf)
        }
        Integer::N32(n) => {
            let mut buf = [0u8; 4];
            NetworkEndian::write_u32(&mut buf, n);
            Element::new(Major::Nint, AdnInfo::MORE4, buf)
        }
        Integer::N64(n) => {
            let mut buf = [0u8; 8];
            NetworkEndian::write_u64(&mut buf, n);
            Element::new(Major::Nint, AdnInfo::MORE8, buf)
        }
    };
    vec![element]
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
