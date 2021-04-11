use crate::{
    geom::direction::*,
    types::{data::*, tilemaps::*, tiles::*},
};
use petgraph::EdgeDirection::Incoming;
use std::collections::{HashMap, HashSet};

use pretty_assertions::{assert_eq, assert_ne};

pub fn evaluate(prog: &TilemapProgram, inputs: HashMap<String, Data>) -> TilemapWorld {
    let (graph, connection_info) = program_to_graph(&prog);
    let outputs = outputs(&prog);

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
                    &connection_info,
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
        connection_info,
    );
    world
}

pub fn weak_head_normal_form(
    graph: &Graph,
    prog: &TilemapProgram,
    connection_info: &ConnectionInfo,
    data: Data,
    context: Vec<HashMap<uuid::Uuid, Data>>,
) -> (WhnfData, Vec<ConnectionPath>) {
    match data {
        Data::Whnf(WhnfData::Nothing) => (WhnfData::Nothing, vec![]),
        Data::Whnf(n @ WhnfData::Number(_)) => (n, vec![]),
        Data::Whnf(p @ WhnfData::Product(_)) => (p, vec![]),
        Data::ThunkPure(graph_node, dependency) => {
            /*let inputs = graph.neighbors_directed(graph_node, Incoming);
            let inputs = inputs
                .flat_map(|node| graph.edges(node))
                .filter(|(_, to, (_, _))| to == &graph_node)
                .map(|(from_node, to_node, (connection_from, connection_to))| {
                    (
                        connection_to,
                        (from_node, to_node, (connection_from, connection_to)),
                    )
                })
                .collect::<HashMap<_, _>>();
                */
            let inputs = graph.neighbors_directed(graph_node, Incoming);
            let inputs = inputs
                .flat_map(|node| graph.edges(node))
                .filter(|(_, to, (_, _))| to == &graph_node)
                .map(|(from_node, to_node, (connection_from, connection_to))| {
                    (
                        connection_to,
                        (
                            from_node,
                            connection_from.clone(),
                            connection_info[&(
                                from_node,
                                to_node,
                                (connection_from.clone(), connection_to.clone()),
                            )]
                                .clone(),
                        ),
                    )
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
                        weak_head_normal_form(graph, prog, connection_info, data.clone(), context)
                    } else {
                        panic!("uuid not in context!")
                    }
                }
                GraphNode::Output(_) => {
                    let mut inputs = inputs.into_iter();
                    if let Some((
                        ToConnection::GlobalOutput(output_location),
                        (from_node, from_connection, connection_path),
                    )) = inputs.next()
                    {
                        if let None = inputs.next() {
                            let new_thunk = Data::ThunkPure(
                                from_node,
                                Dependency::from(from_connection.clone()),
                            );
                            let (whnm, mut lasers) = weak_head_normal_form(
                                graph,
                                prog,
                                connection_info,
                                new_thunk,
                                context,
                            );
                            if !whnm.is_nothing() {
                                lasers.push(connection_path);
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
                    let inputs: HashMap<MachineInput, (GraphNode, FromConnection, ConnectionPath)> =
                        inputs
                            .into_iter()
                            .map(|(to_connection, (from_node, from_connection, connection_path))| match to_connection {
                                ToConnection::FunctionInput(input_label) => (input_label.clone(), (from_node, from_connection, connection_path)),
                                _ => panic!(
                                    "Trying to evaluate a block, but it had an input besides function inputs!\nGraph:\n{}", get_graph_str(graph)
                                ),
                            }).collect();
                    if let Dependency::On(desired_output) = dependency {
                        match tile {
                            TileProgramF::Machine(m) => match m {
                                MachineInfo::BuiltIn(built_in, _) => match built_in {
                                    BuiltInMachine::Produce(()) => {
                                        let (from_node, from_connection, connection_path) = inputs.get(&"a".to_owned()).expect("Needed 'a' as an input to the built-in machine 'produce', but it wasn't there >:(").clone();
                                        let data = Data::ThunkBuiltinOp(
                                            Box::new(BuiltInMachine::Produce(Data::from((
                                                from_node,
                                                from_connection.clone(),
                                            )))),
                                            desired_output,
                                        );
                                        let (whnf, mut lasers) = weak_head_normal_form(
                                            graph,
                                            prog,
                                            connection_info,
                                            data,
                                            context,
                                        );
                                        if !whnf.is_nothing() {
                                            lasers.push(connection_path);
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
                        panic!("We're trying to evaluate the output from a block, but we don't have a dependency! {:?}", dependency)
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
            connection_info,
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

pub fn program_to_graph(prog: &TilemapProgram) -> (Graph, ConnectionInfo) {
    fn add_edge(
        graph: &mut Graph,
        from_connections: &HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        to_connections: &HashMap<GridLineDir, (ToConnection, GraphNode)>,
        from: GridLineDir,
        to: GridLineDir,
        path: ConnectionPath,
        connection_info: &mut ConnectionInfo,
    ) {
        let (to_connection, to_node) = to_connections.get(&to).cloned().unwrap_or({
            let (to_vec, to_dir) = to.previous();
            let nothing_node = GraphNode::Nothing((to_vec.x, to_vec.y), to_dir);
            (ToConnection::Nothing(), nothing_node)
        });

        let (from_connection, from_node) = from_connections
            .get(&from)
            .cloned()
            .and_then(|(from_connection, from_node, long)| {
                if long
                    || (!to_connection.is_nothing()
                        && TileLine::new(from.grid_line, to.grid_line).distance == 0)
                {
                    Some((from_connection, from_node))
                } else {
                    None
                }
            })
            .unwrap_or({
                let (from_vec, from_dir) = to.previous();
                let nothing_node = GraphNode::Nothing((from_vec.x, from_vec.y), from_dir);
                (FromConnection::Nothing(), nothing_node)
            });

        if !to_connection.is_nothing() || !from_connection.is_nothing() {
            let connection = (from_connection.clone(), to_connection.clone());
            graph.add_edge(from_node, to_node, connection.clone());
            connection_info.insert((from_node, to_node, connection), path);
        }
    }

    fn add_edge_from(
        prog: &TilemapProgram,
        graph: &mut Graph,
        from_connections: &HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        to_connections: &HashMap<GridLineDir, (ToConnection, GraphNode)>,
        connection_info: &mut ConnectionInfo,
        from: GridLineDir,
        //long: bool,
    ) {
        let (to, path) = prog.lasercast(from);

        add_edge(
            graph,
            from_connections,
            to_connections,
            from,
            to.to_normal(),
            path,
            connection_info,
        );
    }
    fn add_edge_to(
        prog: &TilemapProgram,
        graph: &mut Graph,
        from_connections: &HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        to_connections: &HashMap<GridLineDir, (ToConnection, GraphNode)>,
        connection_info: &mut ConnectionInfo,
        to: GridLineDir,
    ) {
        let (from, path) = prog.lasercast(to);
        add_edge(
            graph,
            from_connections,
            to_connections,
            from.to_normal(),
            to,
            -path,
            connection_info,
        );
    }

    pub fn create_graph_nodes(
        prog: &TilemapProgram,
    ) -> (
        Graph,
        HashMap<GridLineDir, (FromConnection, GraphNode, bool)>,
        HashMap<GridLineDir, (ToConnection, GraphNode)>,
    ) {
        let mut graph: Graph = Graph::new();
        let mut connection_info = ConnectionInfo::new();

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
                            (ToConnection::FunctionInput(label), current_node),
                        )
                    })
                    .collect::<HashMap<_, _>>();

                let output_info = outputs
                    .into_iter()
                    .map(|((label, long), grid_line_dir)| {
                        (
                            grid_line_dir,
                            (FromConnection::FunctionOutput(label), current_node, long),
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
    ) -> ConnectionInfo {
        let mut connection_info = ConnectionInfo::new();

        // Create edges for inputs
        for (input_index, _) in prog.inputs.iter().enumerate() {
            add_edge_from(
                prog,
                graph,
                from_connections,
                to_connections,
                &mut connection_info,
                prog.get_input_grid_line_dir(input_index),
            );
        }

        // Create edges for outputs
        for (output_index, _) in prog.outputs.iter().enumerate() {
            add_edge_to(
                prog,
                graph,
                from_connections,
                to_connections,
                &mut connection_info,
                prog.get_output_grid_line_dir(output_index),
            );
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
                    &mut connection_info,
                    to_calc_input_to,
                );
            }
            for ((_, _long), to_calc_output_for) in outputs {
                add_edge_from(
                    prog,
                    graph,
                    from_connections,
                    to_connections,
                    &mut connection_info,
                    to_calc_output_for,
                );
            }
        }

        connection_info
    }

    let (mut graph, from_connections, to_connections) = create_graph_nodes(prog);
    let connection_info = create_edges(&prog, &mut graph, &from_connections, &to_connections);

    assert_eq!(
        graph
            .all_edges()
            .map(|(from_node, to_node, (from_connection, to_connection))| (
                from_node,
                to_node,
                (from_connection.clone(), to_connection.clone())
            ))
            .collect::<HashSet<_>>(),
        connection_info.keys().cloned().collect()
    );

    (graph, connection_info)
}

pub fn outputs(prog: &TilemapProgram) -> Vec<uuid::Uuid> {
    prog.outputs
        .iter()
        .map(|(uuid, _, _)| uuid)
        .cloned()
        .collect()
}

pub type AllConnections = Vec<(GraphNode, GraphNode, (FromConnection, ToConnection))>;

pub fn get_all_connections(prog: &TilemapProgram) -> ConnectionInfo {
    let (_, connection_info) = program_to_graph(prog);
    connection_info
}

pub fn get_graph_str(graph: &Graph) -> String {
    use petgraph::dot::Dot;

    format!("{:?}", Dot::with_config(&graph, &[]))
}
