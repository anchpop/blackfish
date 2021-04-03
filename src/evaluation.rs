use crate::{
    geom::{direction::*, tilemap::RaycastHit},
    types::{data::*, tilemaps::*, tiles::*},
};
use petgraph::EdgeDirection::Incoming;
use std::collections::{HashMap, HashSet};

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
                            if !whnm.is_nothing() {
                                lasers.push(TileLineDir::new(
                                    from_connection.loc(prog).grid_line,
                                    prog.get_output_grid_line_dir(*output_location).grid_line,
                                ));
                            }
                            (whnm, lasers)
                        } else {
                            panic!(
                                "Global function output needs to have exactly one input, has more!\nGraph:\n{}", get_graph_str(graph)
                            )
                        }
                    } else {
                        panic!("Global function output needs to have exactly one input, has 0!\nGraph:\n{}", get_graph_str(graph))
                    }
                }
                GraphNode::Block(_, _, tile) => {
                    let inputs: HashMap<_, (GridLineDir, (GraphNode, FromConnection))> =
                        inputs
                            .into_iter()
                            .map(|(to_connection, input)| match to_connection {
                                ToConnection::FunctionInput(to_connection_loc, input_label) => (input_label, (to_connection_loc.clone(), input)),
                                _ => panic!(
                                    "Trying to evaluate a block, but it had an input besides function inputs!\nGraph:\n{}", get_graph_str(graph)
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
                                        if !whnf.is_nothing() {
                                            lasers.push(TileLineDir::new(
                                                input.1.loc(prog).clone().grid_line,
                                                to_connection_loc.grid_line,
                                            ));
                                        }
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
    fn add_edge(
        graph: &mut Graph,
        from_connections: &HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        to_connections: &HashMap<GridLineDir, (ToConnection, GraphNode)>,
        from: GridLineDir,
        to: GridLineDir,
    ) {
        let (from_connection, from_node, long) = from_connections
            .get(&from)
            .cloned()
            .and_then(|(from_connection, from_node, long)| {
                if long || TileLine::new(from.grid_line, to.grid_line).distance == 0 {
                    Some((from_connection, from_node, long))
                } else {
                    None
                }
            })
            .unwrap_or({
                let (from_vec, from_dir) = to.previous();
                let nothing_node = GraphNode::Nothing((from_vec.x, from_vec.y), from_dir);
                (FromConnection::Nothing(to), nothing_node, true)
            });
        let (to_connection, to_node) = to_connections.get(&to).cloned().unwrap_or({
            let (to_vec, to_dir) = to.previous();
            let nothing_node = GraphNode::Nothing((to_vec.x, to_vec.y), to_dir);
            (ToConnection::Nothing(to), nothing_node)
        });

        graph.add_edge(from_node, to_node, (from_connection, to_connection));
    }

    fn add_edge_from(
        prog: &TilemapProgram,
        graph: &mut Graph,
        from_connections: &HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        to_connections: &HashMap<GridLineDir, (ToConnection, GraphNode)>,
        from: GridLineDir,
        //long: bool,
    ) {
        let to = prog.spec.raycast(from).to_normal();
        add_edge(graph, from_connections, to_connections, from, to);
    }
    fn add_edge_to(
        prog: &TilemapProgram,
        graph: &mut Graph,
        from_connections: &HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        to_connections: &HashMap<GridLineDir, (ToConnection, GraphNode)>,
        to: GridLineDir,
    ) {
        let from = prog.spec.raycast(to).to_normal();
        add_edge(graph, from_connections, to_connections, from, to)
    }

    pub fn create_graph_nodes(
        prog: &TilemapProgram,
    ) -> (
        Graph,
        HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        HashMap<GridLineDir, (ToConnection, GraphNode)>,
    ) {
        let mut graph: Graph = Graph::new();

        // inputs
        let global_input_grid_line_dirs: HashMap<GridLineDir, (FromConnection, GraphNode, bool)> =
            prog.inputs
                .iter()
                .map(|(uuid, _, _)| {
                    let index: InputIndex = prog
                        .inputs
                        .iter()
                        .position(|(input_uuid, _, _)| uuid == input_uuid)
                        .unwrap_or_else(|| {
                            panic!(
                                "Input uuid {:?} was not in program's input list {:?}",
                                uuid, prog.inputs
                            )
                        });

                    let node = GraphNode::Input(uuid.clone());

                    let node = graph.add_node(node.clone());

                    (
                        prog.get_input_grid_line_dir(index),
                        (FromConnection::GlobalInput(index), node, true),
                    )
                })
                .collect();

        // outputs
        let global_output_grid_line_dirs: HashMap<GridLineDir, (ToConnection, GraphNode)> = prog
            .outputs
            .iter()
            .map(|(uuid, _, _)| {
                let index: OutputIndex = prog
                    .outputs
                    .iter()
                    .position(|(output_uuid, _, _)| uuid == output_uuid)
                    .unwrap_or_else(|| {
                        panic!(
                            "output uuid {:?} was not in program's output list {:?}",
                            uuid, prog.outputs
                        )
                    });

                let node = GraphNode::Output(uuid.clone());

                let node = graph.add_node(node.clone());

                (
                    prog.get_output_grid_line_dir(index),
                    (ToConnection::GlobalOutput(index), node),
                )
            })
            .collect();

        #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
        enum InOrOut {
            In((ToConnection, GraphNode)),
            Out((FromConnection, GraphNode, bool)),
        }
        let machine_io_grid_line_dirs: HashMap<GridLineDir, InOrOut> = prog
            .spec
            .tiles
            .values()
            .flat_map(|tile_info| {
                let current_node = graph.add_node(GraphNode::new(tile_info.clone()));

                let (location, orientation, tile) = tile_info;

                let tile_positions = prog.spec.get_tile_positions(location, orientation, tile);
                let tile_positions = tile_positions.expect("Invalid tile somehow >:(");
                let inputs = TileProgram::get_inputs(tile_positions.clone());
                let outputs = TileProgram::get_outputs(tile_positions);

                let input_info = inputs
                    .into_iter()
                    .map(|(label, grid_line_dir)| {
                        (
                            grid_line_dir,
                            (
                                ToConnection::FunctionInput(grid_line_dir, label),
                                current_node,
                            ),
                        )
                    })
                    .collect::<HashMap<_, _>>();

                let output_info = outputs
                    .into_iter()
                    .map(|((label, long), grid_line_dir)| {
                        (
                            grid_line_dir,
                            (
                                FromConnection::FunctionOutput(grid_line_dir, label),
                                current_node,
                                long,
                            ),
                        )
                    })
                    .collect::<HashMap<_, _>>();

                // Could have just made the HashSet directly, but I wanted to assert that there were no inputs that were also outputs or w/e
                let relevant_grid_line_dirs = {
                    let v = input_info
                        .keys()
                        .chain(output_info.keys())
                        .cloned()
                        .collect::<Vec<_>>();
                    let s = v.iter().cloned().collect::<HashSet<_>>();
                    assert_eq!(v.len(), s.len());
                    s
                };

                relevant_grid_line_dirs
                    .into_iter()
                    .map(move |grid_line_dir| {
                        (
                            grid_line_dir,
                            if let Some(info) = input_info.get(&grid_line_dir) {
                                InOrOut::In(info.clone())
                            } else {
                                InOrOut::Out(output_info.get(&grid_line_dir).unwrap().clone())
                            },
                        )
                    })
            })
            .collect();

        let machine_inputs = machine_io_grid_line_dirs.iter().filter_map(
            |(grid_line_dir, in_or_out)| match in_or_out {
                InOrOut::In(a) => Some((grid_line_dir.clone(), a.clone())),
                InOrOut::Out(_) => None,
            },
        );

        let machine_outputs = machine_io_grid_line_dirs.iter().filter_map(
            |(grid_line_dir, in_or_out)| match in_or_out {
                InOrOut::In(_) => None,
                InOrOut::Out(a) => Some((grid_line_dir.clone(), a.clone())),
            },
        );

        let all_from_connections = global_input_grid_line_dirs
            .into_iter()
            .chain(machine_outputs)
            .collect();

        let all_to_connections = global_output_grid_line_dirs
            .into_iter()
            .chain(machine_inputs)
            .collect();

        (graph, all_from_connections, all_to_connections)
    }

    pub fn create_edges(
        prog: &TilemapProgram,
        graph: &mut Graph,
        from_connections: &HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        to_connections: &HashMap<GridLineDir, (ToConnection, GraphNode)>,
    ) {
        // Create edges for inputs
        for (input_index, _) in prog.inputs.iter().enumerate() {
            add_edge_from(
                prog,
                graph,
                from_connections,
                to_connections,
                prog.get_input_grid_line_dir(input_index),
                //true,
            )
        }

        // Create edges for outputs
        for (output_index, _) in prog.outputs.iter().enumerate() {
            add_edge_to(
                prog,
                graph,
                from_connections,
                to_connections,
                prog.get_output_grid_line_dir(output_index),
            )
        }

        // Create edges for machines :p
        for (_, tile_info) in prog.spec.tiles.iter() {
            let (location, orientation, tile) = tile_info;

            let tile_positions = prog.spec.get_tile_positions(location, orientation, tile);
            let tile_positions = tile_positions.expect("Invalid tile somehow >:(");
            let inputs = TileProgram::get_inputs(tile_positions.clone());
            let outputs = TileProgram::get_outputs(tile_positions);
            for (_, to_calc_input_to) in inputs {
                add_edge_to(
                    prog,
                    graph,
                    from_connections,
                    to_connections,
                    to_calc_input_to,
                );
            }
            for ((_, long), to_calc_output_for) in outputs {
                add_edge_from(
                    prog,
                    graph,
                    from_connections,
                    to_connections,
                    to_calc_output_for,
                    //long,
                );
            }
        }
    }

    let (mut graph, from_connections, to_connections) = create_graph_nodes(prog);
    create_edges(&prog, &mut graph, &from_connections, &to_connections);

    (
        graph,
        prog.outputs
            .iter()
            .map(|(uuid, _, _)| uuid)
            .cloned()
            .collect(),
    )
}

pub type AllConnections = Vec<(GraphNode, GraphNode, (FromConnection, ToConnection))>;

pub fn get_all_connections(prog: &TilemapProgram) -> AllConnections {
    let (graph, _) = program_to_graph(prog);
    graph
        .all_edges()
        .map(|(f, t, c)| (f, t, c.clone()))
        .collect()
}

pub fn get_graph_str(graph: &Graph) -> String {
    use petgraph::dot::{Config, Dot};

    format!("{:?}", Dot::with_config(&graph, &[]))
}
