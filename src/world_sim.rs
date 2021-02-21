use crate::types::*;

pub fn sim(prog: TilemapProgram) -> TilemapWorld {
    TilemapWorld {
        map: prog.map.map(|a| a.clone().map(TileWorld::Prog)),
    }
}
