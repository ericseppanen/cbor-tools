use crate::{CborType, Indefinite};

/// Create an indefinite-length array.
pub fn make_indef_array<T>(list: Vec<T>) -> CborType
where
    T: Into<CborType>,
{
    // build a regular array struct and then cannibalize it.
    let regular_array = CborType::from(list);
    if let CborType::Array(a) = regular_array {
        CborType::Indefinite(Indefinite::Array(a))
    } else {
        unreachable!()
    }
}

/// Create an indefinite-length map.
pub fn make_indef_map<K, V>(list: Vec<(K, V)>) -> CborType
where
    K: Into<CborType>,
    V: Into<CborType>,
{
    // build a regular map struct and then cannibalize it.
    let regular_map = CborType::from(list);
    if let CborType::Map(m) = regular_map {
        CborType::Indefinite(Indefinite::Map(m))
    } else {
        unreachable!()
    }
}
