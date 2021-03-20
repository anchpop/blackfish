use std::collections::HashMap;

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

    let simmed: TilemapWorld = todo!(); //simulate_until_stable(prog.into_world(inputs));
    let mut output_map: HashMap<uuid::Uuid, Data> = HashMap::new();
    for (index, (uuid, _, data_type)) in outputs_spec.iter().enumerate() {
        let location = Vec2::new(width - 1, index);
        if let Some(data) = simmed.get_outputs(location).and_then(|dirmap| dirmap.east) {
            if data.check_types(data_type) {
                output_map.insert(uuid.clone(), data);
            }
        }
    }
    (simmed, output_map)
}
