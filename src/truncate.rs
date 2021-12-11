pub trait Truncate<T> {
    fn truncate(self) -> T;
}

pub trait TruncateFrom<T> {
    fn truncate_from(x: T) -> Self;
}

impl<Source, Dest> TruncateFrom<Source> for Dest
where
    Source: Truncate<Dest>,
{
    fn truncate_from(x: Source) -> Self {
        let result: Self = x.chop();
        result
    }
}

macro_rules! make_truncate {
    ($Source: ty, $Dest:ty) => {
        impl Truncate<$Dest> for $Source {
            #[track_caller]
            fn truncate(self) -> $Dest {
                use std::convert::TryFrom;

                match <$Dest>::try_from(self) {
                    Ok(val) => val,
                    Err(_) => panic!("truncate overflow"),
                }
            }
        }
    };
}

make_truncate!(usize, u8);
make_truncate!(usize, u16);

make_truncate!(u64, u8);
make_truncate!(u64, u16);
make_truncate!(u64, u32);
make_truncate!(u32, u8);
make_truncate!(u32, u16);
make_truncate!(u16, u8);

make_truncate!(i128, i64);
make_truncate!(i64, i8);
make_truncate!(i64, i16);
make_truncate!(i64, i32);
make_truncate!(i32, i8);
make_truncate!(i32, i16);
make_truncate!(i16, i8);

make_truncate!(i128, u64);
make_truncate!(i64, u8);
make_truncate!(i64, u16);
make_truncate!(i64, u32);
make_truncate!(i32, u8);
make_truncate!(i32, u16);
make_truncate!(i16, u8);
