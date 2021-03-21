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

    let known_map: HashMap<GridLineDir, Data> = inputs
        .iter()
        .map(|(uuid, data)| {
            let index = prog
                .inputs
                .iter()
                .position(|(input_uuid, _, _)| uuid == input_uuid)
                .expect("Input uuid was not in program's uuid list") as i64;
            (
                GridLineDir::new(Vec2i::new(-1, index), Dir::east),
                data.clone(),
            )
        })
        .collect();

    todo!()
}

fn force(
    prog: &TilemapProgram,
    to_calc_input_to: GridLineDir,
    known: HashMap<GridLineDir, Data>,
) -> HashMap<GridLineDir, Data> {
    let raycast_hit = prog.spec.raycast(-to_calc_input_to);
    let output_to_get = raycast_hit.0;
    if known.contains_key(&output_to_get) {}
    todo!()
}
