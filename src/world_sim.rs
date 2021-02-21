use crate::types::*;

pub fn sim(prog: TilemapProgram) -> TilemapWorld {
    TilemapWorld {
        world: prog.program.mapv(|a| a.map(TileWorld::Prog)),
    }
}
