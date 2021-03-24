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

pub fn weak_head_normal_form(
    graph: &Graph,
    data: Data,
    context: Vec<HashMap<uuid::Uuid, Data>>,
) -> (Data, Vec<(GridLineDir, GridLineDir)>) {
    match data {
        Data::Nothing => (Data::Nothing, vec![]),
        Data::ThunkPure(graph_node, dependency) => {
            let inputs = graph.neighbors_directed(graph_node, Incoming);
            println!(
                "all incoming inputs: {:?}",
                graph
                    .neighbors_directed(graph_node, Incoming)
                    .collect::<Vec<_>>()
            );
            let inputs = inputs
                .flat_map(|node| graph.edges(node))
                .filter(|(_, to, (_, _))| to == &graph_node)
                .map(|(node_from, _, (connection_from, connection_to))| {
                    (connection_to, (node_from, connection_from.clone()))
                })
                .collect::<HashMap<_, _>>();

            match &graph_node {
                GraphNode::Input(uuid) => {
                    assert!(inputs.len() == 0, "Input node somehow has an input!");
                    assert!(
                        context.len() == 1,
                        "Currently only support contexts with one thing in them!"
                    );
                    if let Some(data) = context[0].get(uuid) {
                        weak_head_normal_form(graph, data.clone(), context)
                    } else {
                        panic!("uuid not in context!")
                    }
                }
                GraphNode::Output(_) => {
                    let mut inputs = inputs.into_iter();
                    if let Some((
                        ToConnection::GlobalOutput(output_location),
                        (from_node, from_connection),
                    )) = inputs.next()
                    {
                        if let None = inputs.next() {
                            let new_thunk = Data::ThunkPure(
                                from_node,
                                Dependency::from(from_connection.clone()),
                            );
                            let (whnm, mut lasers) =
                                weak_head_normal_form(graph, new_thunk, context);
                            lasers.push((from_connection.loc().clone(), output_location.clone()));
                            (whnm, lasers)
                        } else {
                            panic!(
                                "Global function output needs to have exactly one input, has more!"
                            )
                        }
                    } else {
                        panic!("Global function output needs to have exactly one input, has 0!")
                    }
                }
                GraphNode::Block(_, _, tile) => {
                    assert!(
                        inputs.len() > 0,
                        "Trying to evaluate a block but we somehow found no inputs on the graph! Inputs: {:?}",
                        &inputs
                    );
                    let inputs: HashMap<_, (GridLineDir, (GraphNode, FromConnection))> =
                        inputs
                            .into_iter()
                            .map(|(to_connection, input)| match to_connection {
                                ToConnection::FunctionInput(to_connection_loc, input_label) => (input_label, (to_connection_loc.clone(), input)),
                                _ => panic!(
                                    "Trying to evaluate a block, but it had an input besides function inputs!"
                                ),
                            }).collect();
                    if let Dependency::On(desired_output) = dependency {
                        match tile {
                            TileProgramF::Machine(m) => match m {
                                MachineInfo::BuiltIn(built_in, _) => match built_in {
                                    BuiltInMachine::Produce(()) => {
                                        let (to_connection_loc, input) = inputs.get(&"a".to_owned()).expect("Needed 'a' as an input to the built-in machine 'produce', but it wasn't there >:(").clone();
                                        let data = Data::ThunkBuiltinOp(
                                            Box::new(BuiltInMachine::Produce(Data::from(
                                                input.clone(),
                                            ))),
                                            desired_output,
                                        );
                                        let (whnf, mut lasers) =
                                            weak_head_normal_form(graph, data, context);
                                        lasers.push((input.1.loc().clone(), to_connection_loc));
                                        (whnf, lasers)
                                    }
                                    BuiltInMachine::Iffy((), (), ()) => {
                                        todo!()
                                    }
                                    BuiltInMachine::Trace(()) => {
                                        todo!()
                                    }
                                },
                            },
                        }
                    } else {
                        panic!("We're trying to evaluate the output from a block, but we don't have a dependency!")
                    }
                }
                GraphNode::Nothing(_, _) => {
                    assert!(inputs.len() == 0, "Nothing node somehow has an input!");
                    todo!()
                }
            }
        }
        Data::Number(n) => (Data::Number(n), vec![]),
        Data::ThunkBuiltinOp(op, output) => weak_head_normal_form(
            graph,
            match *op {
                BuiltInMachine::Iffy(_, _, _) => {
                    todo!()
                }
                BuiltInMachine::Trace(_) => {
                    todo!()
                }
                BuiltInMachine::Produce(a) => a,
            },
            context,
        ),
    }
}

pub fn program_to_graph(prog: &TilemapProgram) -> (Graph, Vec<uuid::Uuid>) {
    fn add_node(
        graph: &mut Graph,
        prog: &TilemapProgram,
        to_calc_input_to: GridLineDir,
        to_calc_input_to_node: GraphNode,
        global_inputs: &HashMap<GridLineDir, GraphNode>,
        input_type: ToConnection,
    ) -> FromConnection {
        let raycast_hit = prog.spec.raycast(to_calc_input_to);
        match raycast_hit {
            RaycastHit::HitBorder(hit_normal) => {
                if let Some(hit_node) = global_inputs.get(&hit_normal) {
                    let from_connection = FromConnection::GlobalInput(hit_normal);
                    graph.add_edge(
                        hit_node.clone(),
                        to_calc_input_to_node,
                        (from_connection.clone(), input_type),
                    );
                    from_connection
                } else {
                    let from_connection = FromConnection::Nothing(hit_normal);
                    let hit_node = graph.add_node(GraphNode::nothing(hit_normal));
                    graph.add_edge(
                        hit_node,
                        to_calc_input_to_node,
                        (from_connection.clone(), input_type),
                    );
                    from_connection
                }
            }
            RaycastHit::HitTile(hit_location, normal, (block_center, block_orientation, block)) => {
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
                        let from_connection =
                            FromConnection::FunctionOutput(hit_normal, name.clone());
                        let hit_node = graph.add_node(GraphNode::new((
                            block_center.clone(),
                            block_orientation.clone(),
                            block.clone(),
                        )));
                        graph.add_edge(
                            hit_node,
                            to_calc_input_to_node,
                            (from_connection.clone(), input_type),
                        );
                        from_connection
                    }
                    _ => {
                        let from_connection = FromConnection::Nothing(hit_normal);
                        let hit_node = graph.add_node(GraphNode::nothing(hit_normal));
                        graph.add_edge(
                            hit_node,
                            to_calc_input_to_node,
                            (from_connection.clone(), input_type),
                        );
                        from_connection
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
            add_node(
                &mut graph,
                prog,
                to_calc_input_to,
                current_node,
                &input_grid_line_dirs,
                ToConnection::FunctionInput(to_calc_input_to.clone(), input_name.clone()),
            );
        }
    }

    let outputs = outputs
        .iter()
        .map(|(uuid, (to_calc_input_to, current_node))| {
            add_node(
                &mut graph,
                prog,
                *to_calc_input_to,
                *current_node,
                &input_grid_line_dirs,
                ToConnection::GlobalOutput(to_calc_input_to.clone()),
            );
            uuid.clone()
        })
        .collect();

    (graph, outputs)
}
