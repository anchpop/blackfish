use std::{
    borrow::{Borrow, BorrowMut},
    collections::HashMap,
};

use crate::geom::direction::*;
use crate::geom::tilemap::RaycastHit;
use crate::geom::*;
use crate::types::data::*;
use crate::types::tilemaps::*;
use crate::types::tiles::*;
use crate::types::*;
use bevy::ecs::Location;
use frunk::monoid::Monoid;
use std::collections::hash_map::{Entry, Entry::Occupied, OccupiedEntry};

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

    let mut known: HashMap<GridLineDir, Data> = inputs
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

    let outputs: HashMap<uuid::Uuid, Data> = prog
        .outputs
        .iter()
        .enumerate()
        .map(|(index, (uuid, _, _))| {
            println!("");
            let data = force(
                &prog,
                GridLineDir::new(Vec2::new(width - 1, index), Dir::east),
                &mut known,
            );
            (uuid.clone(), data)
        })
        .collect();

    let world_inputs = prog
        .inputs
        .iter()
        .map(|(uuid, name, typ)| (name.clone(), inputs.get(uuid).cloned()))
        .collect();

    let world_outputs = prog
        .outputs
        .iter()
        .map(|(uuid, name, typ)| (name.clone(), outputs.get(uuid).cloned()))
        .collect();

    return (prog.into_world(world_inputs, world_outputs), outputs);
}

fn force(
    prog: &TilemapProgram,
    to_calc_input_to: GridLineDir,
    known: &mut HashMap<GridLineDir, Data>,
) -> Data {
    let raycast_hit = prog.spec.raycast(-to_calc_input_to);
    match raycast_hit {
        RaycastHit::HitBorder(hit_normal) => {
            // Could be hitting an input - if so, it should be in the known map
            if let Some(value) = known.get(&hit_normal).cloned() {
                known.insert(to_calc_input_to, value.clone());
                value
            } else {
                Data::Nothing(hit_normal)
            }
        }
        RaycastHit::HitTile(hit_location, dir, tile_data) => {
            let tile_positions =
                prog.spec
                    .get_tile_positions(&tile_data.0, &tile_data.1, &tile_data.2);
            let tile_positions = tile_positions.expect("Invalid tile somehow >:(");
            let io_map = tile_positions.get(&hit_location);
            todo!()
        }
    }
}
