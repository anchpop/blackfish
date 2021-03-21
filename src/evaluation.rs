use std::collections::HashMap;

use crate::geom::direction::*;
use crate::geom::*;
use crate::types::data::*;
use crate::types::tilemaps::*;
use crate::types::tiles::*;
use crate::types::*;
use bevy::ecs::Location;
use frunk::monoid::Monoid;

pub fn evaluate(
    prog: TilemapProgram,
    inputs: HashMap<uuid::Uuid, Data>,
) -> (TilemapWorld, HashMap<uuid::Uuid, Data>) {
    let width = prog.program_dim().w;
    let outputs_spec = prog.outputs.clone();

    let mut output_map: HashMap<uuid::Uuid, Data> = HashMap::new();

    let output_spot = GridLineDir {
        grid_line: GridLine::new(Vec2::new(width - 1, 0), Dir::east),
        direction: Sign::Positive,
    };

    todo!()
}

fn get_output_from_grid_line_dir(
    spot: GridLineDir,
    known: HashMap<GridLineDir, Data>,
) -> HashMap<GridLineDir, Data> {
    todo!()
}
