use uom::fmt::DisplayStyle::*;
use uom::si::length::meter;

#[macro_use]
pub mod music_time {
    quantity! {
        /// Time (base unit second, s).
        quantity: MusicTime; "music time";
        /// Time dimension, s.
        dimension: Q<P1>; // time
        units {
            @demisemihemidemisemiquaver: 1.0; "d", "demisemihemidemisemiquaver", "demisemihemidemisemiquavers";
            @bang: 8.0; "ba", "bang", "bangs";
            @beat: 64.0; "be", "beat", "beats";
            @note: 256.0; "n", "note", "notes";
        }
    }
}

system! {
    quantities: Q {
        music_time: demisemihemidemisemiquaver, T;
    }

    units: U {
        mod music_time::MusicTime,
    }
}

pub mod f64 {
    mod mks {
        pub use super::super::*;
    }

    Q!(self::mks, f64);
}
