use std::collections::HashMap;

use crate::geom::direction::*;
use crate::geom::tilemap::RaycastHit;

use crate::types::data::*;
use crate::types::tilemaps::*;
use crate::types::tiles::*;

use petgraph::EdgeDirection::Incoming;

pub fn evaluate(prog: &TilemapProgram, inputs: HashMap<String, Data>) -> TilemapWorld {
    let (graph, outputs) = program_to_graph(&prog);
    let prog_input_labels: HashMap<uuid::Uuid, MachineInput> = prog
        .inputs
        .clone()
        .into_iter()
        .map(|(uuid, label, _)| (uuid, label))
        .collect();
    let prog_output_labels: HashMap<uuid::Uuid, MachineOutput> = prog
        .outputs
        .clone()
        .into_iter()
        .map(|(uuid, label, _)| (uuid, label))
        .collect();

    let output_nodes = outputs
        .into_iter()
        .map(|uuid| (uuid, GraphNode::Output(uuid)));
    let outputs = output_nodes
        .map(|(uuid, node)| {
            (
                uuid,
                weak_head_normal_form(
                    &graph,
                    &prog,
                    Data::ThunkPure(node, Dependency::Only),
                    vec![prog
                        .inputs
                        .iter()
                        .map(|(uuid, label, _)| (uuid.clone(), inputs.get(label).unwrap().clone()))
                        .collect()],
                ),
            )
        })
        .collect::<Vec<_>>();

    let labeled_outputs = outputs
        .clone()
        .into_iter()
        .map(|(uuid, (data, _))| {
            (
                prog_output_labels
                    .get(&uuid)
                    .unwrap_or_else(|| {
                        panic!("Couldn't find {:?} in {:?}", &uuid, &prog_input_labels)
                    })
                    .clone(),
                Data::Whnf(data),
            )
        })
        .collect();

    let world = prog.clone().into_world(
        prog.inputs
            .iter()
            .map(|(_uuid, label, _datatype)| (label.clone(), inputs.get(label).cloned()))
            .collect(),
        labeled_outputs,
        outputs
            .into_iter()
            .flat_map(|(_, (_, lasers))| lasers)
            .collect(),
    ); // should probably add the actual inputs and outputs here
    world
}

pub fn weak_head_normal_form(
    graph: &Graph,
    prog: &TilemapProgram,
    data: Data,
    context: Vec<HashMap<uuid::Uuid, Data>>,
) -> (WhnfData, Vec<TileLineDir>) {
    match data {
        Data::Whnf(WhnfData::Nothing) => (WhnfData::Nothing, vec![]),
        Data::Whnf(n @ WhnfData::Number(_)) => (n, vec![]),
        Data::Whnf(p @ WhnfData::Product(_)) => (p, vec![]),
        Data::ThunkPure(graph_node, dependency) => {
            let inputs = graph.neighbors_directed(graph_node, Incoming);
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
                        weak_head_normal_form(graph, prog, data.clone(), context)
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
                                weak_head_normal_form(graph, prog, new_thunk, context);
                            lasers.push(TileLineDir::new(
                                from_connection.loc(prog).grid_line,
                                prog.get_output_grid_line_dir(*output_location).grid_line,
                            ));
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
                                            weak_head_normal_form(graph, prog, data, context);
                                        lasers.push(TileLineDir::new(
                                            input.1.loc(prog).clone().grid_line,
                                            to_connection_loc.grid_line,
                                        ));
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
                            TileProgramF::Literal(l) => {
                                (WhnfData::from(prog.constants[*l].clone()), vec![])
                            }
                        }
                    } else {
                        panic!("We're trying to evaluate the output from a block, but we don't have a dependency!")
                    }
                }
                GraphNode::Nothing(_, _) => {
                    assert!(inputs.len() == 0, "Nothing node somehow has an input!");
                    (WhnfData::Nothing, vec![])
                }
            }
        }
        Data::ThunkBuiltinOp(op, _output) => weak_head_normal_form(
            graph,
            prog,
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
        global_inputs: &HashMap<InputIndex, GraphNode>,
        input_type: ToConnection,
    ) -> FromConnection {
        let raycast_hit = prog.spec.raycast(to_calc_input_to);
        match raycast_hit {
            RaycastHit::HitBorder(hit_normal) => {
                if let Some(input_index) = prog.check_input_grid_line_dir(hit_normal) {
                    let hit_node = global_inputs
                        .get(&input_index)
                        .expect("Input not provided to program_to_graph");

                    let from_connection = FromConnection::GlobalInput(input_index);
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

                match (
                    io_map.get(&normal),
                    hit_normal.grid_line.distance(&to_calc_input_to.grid_line),
                ) {
                    (Some(IOType::OutLong(name)), _) | (Some(IOType::OutShort(name)), 0) => {
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

    let _width = prog.program_dim().w;
    let mut graph: Graph = Graph::new();

    // Only nodes in the graph are the inputs, outputs, and machines. Let's just add them all straightaway.

    // inputs
    let input_grid_line_dirs: HashMap<InputIndex, GraphNode> = prog
        .inputs
        .iter()
        .map(|(uuid, _, _)| {
            let index = prog
                .inputs
                .iter()
                .position(|(input_uuid, _, _)| uuid == input_uuid)
                .expect("Input uuid was not in program's uuid list");

            let node = GraphNode::Input(uuid.clone());

            let node = graph.add_node(node.clone());

            (index, node)
        })
        .collect();

    // outputs
    let outputs: HashMap<uuid::Uuid, (OutputIndex, GraphNode)> = prog
        .outputs
        .iter()
        .enumerate()
        .map(|(index, (uuid, _, _))| {
            let node = GraphNode::Output(uuid.clone());
            let node = graph.add_node(node.clone());
            (uuid.clone(), (index, node))
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
        .map(|(uuid, (to_calc_output_to, current_node))| {
            add_node(
                &mut graph,
                prog,
                prog.get_output_grid_line_dir(*to_calc_output_to),
                *current_node,
                &input_grid_line_dirs,
                ToConnection::GlobalOutput(*to_calc_output_to),
            );
            uuid.clone()
        })
        .collect();

    (graph, outputs)
}
