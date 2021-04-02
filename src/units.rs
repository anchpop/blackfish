pub mod music_time {
    quantity! {
        quantity: MusicTime; "music time";
        dimension: Q<P1>;
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
