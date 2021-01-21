//! This module provides some optional implementations of the `Display` trait.
//!
//! When debugging CBOR decoding, it can be interesting to look at the "symbolic"
//! view of the raw CBOR data. Implementing `Display` for these data structures
//! allows us to easily examine the low-level CBOR encoding properties.
//!
//! See the examples directory for one way this can be used.

use crate::format::{AdnInfo, Element, ImmediateValue, Major};

impl std::fmt::Display for Major {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.as_ref(), *self as u8)
    }
}

impl std::fmt::Display for ImmediateValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImmediateValue::Empty => Ok(()),
            ImmediateValue::Bytes1(b) => write!(f, "{}", hex_fmt::HexFmt(b)),
            ImmediateValue::Bytes2(b) => write!(f, "{}", hex_fmt::HexFmt(b)),
            ImmediateValue::Bytes4(b) => write!(f, "{}", hex_fmt::HexFmt(b)),
            ImmediateValue::Bytes8(b) => write!(f, "{}", hex_fmt::HexFmt(b)),
        }
    }
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} adn=0x{:x}", self.major, self.adn_info.0)?;

        if self.major == Major::Misc {
            // In major 7, there are a number of special values.
            match self.adn_info {
                AdnInfo::FALSE => write!(f, " (FALSE)")?,
                AdnInfo::TRUE => write!(f, " (TRUE)")?,
                AdnInfo::NULL => write!(f, " (NULL)")?,
                AdnInfo::UNDEFINED => write!(f, " (UNDEFINED)")?,
                AdnInfo::FLOAT16 => write!(f, " (FLOAT16)")?,
                AdnInfo::FLOAT32 => write!(f, " (FLOAT32)")?,
                AdnInfo::FLOAT64 => write!(f, " (FLOAT64)")?,
                AdnInfo::BREAK => write!(f, " (BREAK)")?,
                _ => write!(f, " (???)")?,
            }
        } else {
            match self.adn_info {
                AdnInfo(n) if n < 24 => {
                    write!(f, " (len={})", n)?;
                }
                AdnInfo::INDEFINITE => write!(f, " (INDEFINITE)")?,
                AdnInfo::MORE1 => write!(f, " (1BYTE)")?,
                AdnInfo::MORE2 => write!(f, " (2BYTE)")?,
                AdnInfo::MORE4 => write!(f, " (4BYTE)")?,
                AdnInfo::MORE8 => write!(f, " (8BYTE)")?,
                _ => write!(f, " (???)")?,
            };
        }

        if self.imm != ImmediateValue::Empty {
            write!(f, " <{}>", self.imm)?;
        }

        if !self.bytes.is_empty() {
            write!(f, " [{}]", hex_fmt::HexFmt(&self.bytes))?;
        }
        Ok(())
    }
}
