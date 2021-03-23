use std::{
    borrow::{Borrow, BorrowMut},
    collections::HashMap,
    thread::current,
};

use crate::geom::direction::*;
use crate::geom::tilemap::RaycastHit;
use crate::geom::*;
use crate::types::data::*;
use crate::types::tilemaps::*;
use crate::types::tiles::*;
use crate::types::*;

use frunk::monoid::Monoid;
use std::collections::hash_map::{Entry, Entry::Occupied, OccupiedEntry};

use petgraph::stable_graph::StableGraph;

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
    println!("forcing at {:?}", to_calc_input_to);
    let raycast_hit = prog.spec.raycast(-to_calc_input_to);
    match raycast_hit {
        RaycastHit::HitBorder(hit_normal) => {
            // Could be hitting an input - if so, it should be in the known map
            if let Some(value) = known.get(&hit_normal).cloned() {
                known.insert(to_calc_input_to, value.clone());
                value
            } else {
                Data::Nothing
            }
        }
        RaycastHit::HitTile(hit_location, dir, (tile_center, tile_orientation, tile)) => {
            println!("seems to be dependent on a tile");
            let hit_normal = GridLineDir::new(hit_location, dir);

            let tile_positions = prog
                .spec
                .get_tile_positions(tile_center, tile_orientation, tile);
            let tile_positions = tile_positions.expect("Invalid tile somehow >:(");
            let io_map = tile_positions
                .get(&hit_location)
                .expect("tile hit somehow not in result of calling tile_positions");
            let io_typ = io_map.get(&dir);
            println!("the tile's io type is {:?}", io_typ);

            if let Some(io_typ) = io_typ {
                match io_typ {
                    IOType::In(_) => Data::Nothing,
                    IOType::Out(_) => match tile {
                        TileProgramF::Machine(machine_info) => match machine_info {
                            MachineInfo::BuiltIn(built_in_machine, program_info) => {
                                let inputs = TileProgram::get_inputs(tile_positions);
                                match built_in_machine {
                                    BuiltInMachine::Produce(_) => Data::ThunkBuiltinOp(
                                        Box::new(BuiltInMachine::Produce(Data::ThunkPure(
                                            todo!(), //inputs[&"input".to_owned()].clone(),
                                        ))),
                                        "output".to_owned(),
                                    ),
                                    BuiltInMachine::Iffy(_, _, _) => {
                                        todo!()
                                    }
                                    BuiltInMachine::Trace(_) => {
                                        todo!()
                                    }
                                }
                            }
                        },
                    },
                }
            } else {
                Data::Nothing
            }
        }
    }
}

pub fn program_to_graph(prog: &TilemapProgram) -> Graph {
    fn add_node(
        graph: &mut Graph,
        prog: &TilemapProgram,
        to_calc_input_to: GridLineDir,
        to_calc_input_to_node: GraphNode,
        known_nodes: &HashMap<GridLineDir, GraphNode>,
    ) {
        println!(
            "casting ray to get inputs leading to {:?}",
            &(to_calc_input_to)
        );
        let raycast_hit = prog.spec.raycast(to_calc_input_to);
        match raycast_hit {
            RaycastHit::HitBorder(hit_normal) => {
                println!("hitborder {:?}", &hit_normal);
                let (location, dir) = hit_normal.parts();
                let hit_node = GraphNode::Nothing((location.x, location.y), dir);
                let hit_node = graph.add_node(hit_node);
                graph.add_edge(hit_node, to_calc_input_to_node, ());
            }
            RaycastHit::HitTile(_, _, _) => {
                println!("hit tile");
            }
        }
    }

    let width = prog.program_dim().w;
    let mut graph: Graph = Graph::new();

    // Only nodes in the graph are the inputs, outputs, and machines. Let's just add them all straightaway.

    // inputs
    let known: HashMap<GridLineDir, GraphNode> = prog
        .inputs
        .iter()
        .map(|(uuid, _, _)| {
            let index = prog
                .inputs
                .iter()
                .position(|(input_uuid, _, _)| uuid == input_uuid)
                .expect("Input uuid was not in program's uuid list") as i64;

            let node = GraphNode::Input(uuid.clone());

            let node = graph.add_node(node.clone());

            (GridLineDir::new(Vec2i::new(-1, index), Dir::east), node)
        })
        .collect();

    // outputs
    let outputs: HashMap<uuid::Uuid, (GridLineDir, GraphNode)> = prog
        .outputs
        .iter()
        .enumerate()
        .map(|(index, (uuid, _, _))| {
            let node = GraphNode::Output(uuid.clone());
            let grid_line_dir = GridLineDir::new(Vec2::new(width, index), Dir::west);
            let node = graph.add_node(node.clone());
            (uuid.clone(), (grid_line_dir, node))
        })
        .collect();

    // machines
    for (_, tile_info) in prog.spec.tiles.iter() {
        let current_node = graph.add_node(GraphNode::new(tile_info.clone()));

        // ok, now to add all the edges that lead to the machines
        let (location, orientation, tile) = tile_info;

        let tile_positions = prog.spec.get_tile_positions(location, orientation, tile);
        let tile_positions = tile_positions.expect("Invalid tile somehow >:(");
        let inputs = TileProgram::get_inputs(tile_positions);
        for (input_name, to_calc_input_to) in inputs {
            println!("working on {}", input_name);
            add_node(&mut graph, prog, to_calc_input_to, current_node, &known);
        }
    }

    for (uuid, (to_calc_input_to, current_node)) in outputs.iter() {
        println!("doing output");
        add_node(&mut graph, prog, *to_calc_input_to, *current_node, &known)
    }

    graph
}
