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

use petgraph::{stable_graph::StableGraph, EdgeDirection::Incoming};

pub fn evaluate(
    prog: TilemapProgram,
    inputs: HashMap<uuid::Uuid, Data>,
) -> (TilemapWorld, HashMap<uuid::Uuid, Data>) {
    todo!()
}

fn weak_head_normal_form(graph: Graph, data: Data) -> Data {
    match data {
        Data::Nothing => Data::Nothing,
        Data::ThunkPure(graph_node) => {
            let inputs = graph.neighbors_directed(graph_node, Incoming);
            inputs.filter(|a| true);
            todo!()
        }
        Data::Number(n) => Data::Number(n),
        Data::ThunkBuiltinOp(op, output) => match *op {
            BuiltInMachine::Iffy(_, _, _) => {
                todo!()
            }
            BuiltInMachine::Trace(_) => {
                todo!()
            }
            BuiltInMachine::Produce(a) => a,
        },
    }
}

pub fn program_to_graph(
    prog: &TilemapProgram,
) -> (Graph, HashMap<uuid::Uuid, (GridLineDir, GraphNode)>) {
    fn add_node(
        graph: &mut Graph,
        prog: &TilemapProgram,
        to_calc_input_to: GridLineDir,
        to_calc_input_to_node: GraphNode,
        input_grid_line_dirs: &HashMap<GridLineDir, GraphNode>,
        input_type: ToConnection,
    ) {
        println!(
            "casting ray to get inputs leading to {:?}",
            &(to_calc_input_to)
        );
        let raycast_hit = prog.spec.raycast(to_calc_input_to);
        match raycast_hit {
            RaycastHit::HitBorder(hit_normal) => {
                if let Some(hit_node) = input_grid_line_dirs.get(&hit_normal) {
                    graph.add_edge(
                        hit_node.clone(),
                        to_calc_input_to_node,
                        (FromConnection::GlobalInput, input_type),
                    );
                } else {
                    let hit_node = graph.add_node(GraphNode::nothing(hit_normal));
                    graph.add_edge(
                        hit_node,
                        to_calc_input_to_node,
                        (FromConnection::Nothing, input_type),
                    );
                }
            }
            RaycastHit::HitTile(hit_location, normal, (block_center, block_orientation, block)) => {
                println!("hit tile");
                let hit_normal = GridLineDir::new(hit_location, normal);

                let tile_positions =
                    prog.spec
                        .get_tile_positions(block_center, block_orientation, block);
                let tile_positions = tile_positions.expect("Invalid tile somehow >:(");
                let io_map = tile_positions
                    .get(&hit_location)
                    .expect("tile hit somehow not in result of calling tile_positions");

                match io_map.get(&normal) {
                    Some(IOType::Out(name)) => {
                        let hit_node = graph.add_node(GraphNode::new((
                            block_center.clone(),
                            block_orientation.clone(),
                            block.clone(),
                        )));
                        graph.add_edge(
                            hit_node,
                            to_calc_input_to_node,
                            (FromConnection::FunctionOutput(name.clone()), input_type),
                        );
                    }
                    _ => {
                        let hit_node = graph.add_node(GraphNode::nothing(hit_normal));
                        graph.add_edge(
                            hit_node,
                            to_calc_input_to_node,
                            (FromConnection::Nothing, input_type),
                        );
                    }
                }
            }
        }
    }

    let width = prog.program_dim().w;
    let mut graph: Graph = Graph::new();

    // Only nodes in the graph are the inputs, outputs, and machines. Let's just add them all straightaway.

    // inputs
    let input_grid_line_dirs: HashMap<GridLineDir, GraphNode> = prog
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
            add_node(
                &mut graph,
                prog,
                to_calc_input_to,
                current_node,
                &input_grid_line_dirs,
                ToConnection::FunctionInput(input_name.clone()),
            );
        }
    }

    for (uuid, (to_calc_input_to, current_node)) in outputs.iter() {
        println!("doing output");
        add_node(
            &mut graph,
            prog,
            *to_calc_input_to,
            *current_node,
            &input_grid_line_dirs,
            ToConnection::GlobalOutput,
        )
    }

    (graph, outputs)
}
