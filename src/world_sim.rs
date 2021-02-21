use crate::types::*;

pub fn sim(prog: TilemapProgram) -> TilemapWorld {
    TilemapWorld {
        map: prog.map.mapv(|a| a.map(TileWorld::Prog)),
    }
}
