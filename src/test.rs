use crate::geom::direction::*;

use crate::geom::{Extent2, Vec2};

use crate::types::{data::*, tilemaps::*, tiles::*};

pub fn default_program() -> TilemapProgram {
    in_out_id_with_indirection_prog()
}

pub fn const_prog() -> TilemapProgram {
    let mut prog = in_out_id_prog();
    let data = prog.constants.insert(NfData::Number(10));
    prog.try_do_to_map(|map| {
        let map = map.add(
            Vec2::new(3, 0),
            Dir::EAST,
            TileProgram::Machine(MachineInfo::BuiltIn(
                BuiltInMachine::Produce(()),
                ProgramInfo {},
            )),
        )?;
        map.add(Vec2::new(2, 0), Dir::NORTH, TileProgram::Literal(data))
    })
    .unwrap()
}

pub fn in_out_id_with_indirection_prog() -> TilemapProgram {
    in_out_id_prog()
        .try_do_to_map(|map| {
            map.add(
                Vec2::new(3, 0),
                Dir::EAST,
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {},
                )),
            )
        })
        .unwrap()
}
pub fn in_out_id_blocked_prog() -> TilemapProgram {
    in_out_id_prog()
        .try_do_to_map(|map| {
            map.add(
                Vec2::new(3, 0),
                Dir::default(),
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {},
                )),
            )
        })
        .unwrap()
}

pub fn in_out_id_prog() -> TilemapProgram {
    let clock_uuid = uuid::Uuid::new_v4();
    let audio_uuid = uuid::Uuid::new_v4();

    let mut prog = simple_program();
    prog.inputs = vec![(clock_uuid, "Clock".to_string(), DataType::Number)];
    prog.outputs = vec![(audio_uuid, "Audio".to_string(), DataType::Number)];
    prog
}
pub fn simple_program() -> TilemapProgram {
    let map = empty_program().try_do_to_map(|map| {
        map.add(
            Vec2::new(3, 2),
            Dir::default(),
            TileProgram::Machine(MachineInfo::BuiltIn(
                BuiltInMachine::Produce(()),
                ProgramInfo {},
            )),
        )
    });
    /*
    let west_laser = tiles.insert(TileProgram::Machine(
        Dir::default(),
        MachineInfo::BuiltIn(
            BuiltInMachines::Produce,
            ProgramInfo {
                hardcoded_inputs: btree_map! {
                    "product".to_string(): Data::Number(3)
                },
                ..ProgramInfo::empty()
            },
        ),
    ));
    let west_laser_2 = tiles.insert(TileProgram::Machine(
        Dir::default(),
        MachineInfo::BuiltIn(
            BuiltInMachines::Produce,
            ProgramInfo {
                hardcoded_inputs: btree_map! {
                    "product".to_string(): Data::Number(3)
                },
                ..ProgramInfo::empty()
            },
        ),
    ));

    TilemapProgram::new(Tilemap {
        tiles,
        map: arr2(&[
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [
                None,
                None,
                None,
                Some(north_laser),
                None,
                Some(west_laser),
                None,
                None,
                None,
            ],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(west_laser_2),
                None,
            ],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
        ]),
    }) */
    map.unwrap()
}

fn empty_program() -> TilemapProgram {
    TilemapProgram::new_empty(Extent2::new(10, 10))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    // I want to put algebra-driven design into practice and design a tilemap algebra with proprety-based tests.
    // But for now, I'm just going to write simple unit tests to test things that obviously should work.

    #[cfg(test)]
    mod equality {
        use super::*;
        use pretty_assertions::{assert_eq, assert_ne};
        use velcro::hash_set;

        #[test]
        fn basic_tilemap_equality_empty() {
            assert_eq!(empty_program().spec, empty_program().spec);
        }
        #[test]
        fn basic_tilemap_equality_simple() {
            assert_eq!(simple_program().spec, simple_program().spec);
        }

        #[test]
        fn basic_tilemap_inequality_default_2() {
            assert_ne!(default_program().spec, empty_program().spec);
        }

        #[test]
        fn tilemap_world_equality() {
            assert_eq!(
                default_program().into_world(vec![], vec![], hash_set![]),
                default_program().into_world(vec![], vec![], hash_set![])
            );
        }
        #[test]
        fn tilemap_equality_invariant_to_unreferenced_tiles() {
            let m1 = default_program();
            let mut m2 = m1.clone();
            m2.spec.tiles.insert((
                Vec2::new(0, 0),
                Dir::default(),
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {},
                )),
            ));
            assert_eq!(m1.spec, m2.spec);
        }
    }

    #[cfg(test)]
    mod gridlines {
        use super::*;
        use crate::geom::{Vec2, Vec2i};
        use pretty_assertions::assert_eq;

        #[test]
        fn grid_line_east() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-1, 0),
                    side: Basis::East
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::EAST)
            );
        }
        #[test]
        fn grid_line_north() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-1, 0),
                    side: Basis::North
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::NORTH)
            );
        }
        #[test]
        fn grid_line_south() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-1, -1),
                    side: Basis::North
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::SOUTH)
            );
        }
        #[test]
        fn grid_line_west() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-2, 0),
                    side: Basis::East
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::WEST)
            );
        }

        #[test]
        fn grid_line_dir_east() {
            assert_eq!(
                GridLineDir {
                    grid_line: GridLine {
                        location: Vec2i::new(-1, 0),
                        side: Basis::East
                    },
                    direction: Sign::Positive
                },
                GridLineDir::new(Vec2i::new(-1, 0), Dir::EAST)
            );
        }
        #[test]
        fn grid_line_dir_north() {
            assert_eq!(
                GridLineDir {
                    grid_line: GridLine {
                        location: Vec2i::new(-1, 0),
                        side: Basis::North
                    },
                    direction: Sign::Positive
                },
                GridLineDir::new(Vec2i::new(-1, 0), Dir::NORTH)
            );
        }
        #[test]
        fn grid_line_dir_west() {
            assert_eq!(
                GridLineDir {
                    grid_line: GridLine {
                        location: Vec2i::new(-2, 0),
                        side: Basis::East
                    },
                    direction: Sign::Negative
                },
                GridLineDir::new(Vec2i::new(-1, 0), Dir::WEST)
            );
        }

        #[test]
        fn grid_line_dir_south() {
            assert_eq!(
                GridLineDir {
                    grid_line: GridLine {
                        location: Vec2i::new(-1, -1),
                        side: Basis::North
                    },
                    direction: Sign::Negative
                },
                GridLineDir::new(Vec2i::new(-1, 0), Dir::SOUTH)
            );
        }

        #[test]
        fn grid_line_dir_parts_north() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::NORTH;

            let (recovered_start, _recovered_dir) = GridLineDir::new(start, dir).previous();
            assert_eq!(start, recovered_start);
        }
        #[test]
        fn grid_line_dir_parts_south() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::SOUTH;

            let (recovered_start, _recovered_dir) = GridLineDir::new(start, dir).previous();
            assert_eq!(start, recovered_start);
        }
        #[test]
        fn grid_line_dir_parts_east() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::EAST;

            let (recovered_start, _recovered_dir) = GridLineDir::new(start, dir).previous();
            assert_eq!(start, recovered_start);
        }
        #[test]
        fn grid_line_dir_parts_west() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::WEST;

            let (recovered_start, _recovered_dir) = GridLineDir::new(start, dir).previous();
            assert_eq!(start, recovered_start);
        }
        #[test]
        fn grid_line_dir_next() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::WEST;

            let (recovered_start, recovered_dir) = GridLineDir::new(start, dir).next();
            assert_eq!(dir.shift(start), recovered_start);
            assert_eq!(dir, recovered_dir);
        }
        #[test]
        fn grid_line_dir_advance() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::WEST;

            let (recovered_start, recovered_dir) =
                GridLineDir::new(start, dir).advance(1).previous();
            assert_eq!(dir.shift(start), recovered_start);
            assert_eq!(dir, recovered_dir);
        }

        #[test]
        fn grid_line_dir_directionally_neutral() {
            let loc = Vec2::new(3, 3);
            let dir = Dir::EAST;

            let a = GridLineDir::new(loc, dir);
            let b = GridLineDir::new(dir.shift(loc), -dir);
            assert_eq!(a.grid_line, b.grid_line);
        }
    }

    #[cfg(test)]
    mod tilelines {
        use super::*;
        use crate::geom::{Vec2, Vec2i};
        use pretty_assertions::assert_eq;

        #[test]
        fn tile_line_east() {
            let start = GridLine::new(Vec2i::new(0, 0), Dir::EAST);
            let end = GridLine::new(Vec2i::new(5, 0), Dir::EAST);
            let l = TileLine::new(start, end);
            assert_eq!(l.distance, 5);
            assert_eq!(l.grid_line, start);
            assert_eq!(l, TileLine::new(end, start));
        }
        #[test]
        fn tile_line_west() {
            let start = GridLine::new(Vec2i::new(0, 0), Dir::EAST);
            let end = GridLine::new(Vec2i::new(5, 0), Dir::WEST);
            let l = TileLine::new(start, end);
            assert_eq!(l.distance, 4);
            assert_eq!(l.grid_line, start);
            assert_eq!(l, TileLine::new(end, start));
        }

        #[test]
        fn tile_line_dir_east() {
            let start = GridLine::new(Vec2i::new(0, 0), Dir::EAST);
            let end = GridLine::new(Vec2i::new(5, 0), Dir::EAST);
            let l = TileLineDir::new(start, end);
            assert_eq!(l.sign, Sign::Positive);
        }
        #[test]
        fn tile_line_dir_west() {
            let start = GridLine::new(Vec2i::new(0, 0), Dir::EAST);
            let end = GridLine::new(Vec2i::new(5, 0), Dir::EAST);
            let l = TileLineDir::new(end, start);
            assert_eq!(l.sign, Sign::Negative);
        }
        #[test]
        fn tile_line_dir_iter() {
            let start = GridLine::new(Vec2i::new(0, 0), Dir::EAST);
            let end = GridLine::new(Vec2i::new(5, 0), Dir::EAST);
            let l = TileLineDir::new(start, end);
            assert_eq!(
                l.get_start(),
                GridLineDir {
                    grid_line: start,
                    direction: Sign::Positive
                }
            );
            assert_eq!(
                l.into_iter().collect::<Vec<_>>(),
                vec![
                    Vec2i::new(1, 0),
                    Vec2i::new(2, 0),
                    Vec2i::new(3, 0),
                    Vec2i::new(4, 0),
                    Vec2i::new(5, 0),
                ]
            );
        }

        #[test]
        fn tile_line_contains() {
            let start = GridLine::new(Vec2i::new(0, 0), Dir::EAST);
            let end = GridLine::new(Vec2i::new(5, 0), Dir::EAST);
            let l = TileLine::new(start, end);

            assert!(l.contains(Vec2i::new(4, 0)));
            assert!(l.contains(Vec2i::new(5, 0)));
            assert!(!l.contains(Vec2i::new(0, 0)));
        }

        #[test]
        fn tile_line_0_dist() {
            let gld = GridLine::new(Vec2i::new(0, 0), Dir::EAST);
            assert_eq!(0, TileLine::new(gld, gld).distance);
        }
    }

    #[cfg(test)]
    mod raycast {
        use crate::geom::{tilemap::RaycastHit, Vec2, Vec2i};
        use pretty_assertions::assert_eq;
        use vek::Ray;

        use super::*;

        #[test]
        fn test_ray_hit_edge() {
            let prog = in_out_id_prog();

            let raycast_hit = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(5, 0), Dir::WEST));

            assert_eq!(
                raycast_hit,
                RaycastHit::HitBorder(GridLineDir::new(Vec2i::new(-1, 0), Dir::EAST))
            );
        }

        #[test]
        fn test_ray_hit_machine() {
            let prog = in_out_id_with_indirection_prog();

            let raycast_hit = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(5, 0), Dir::WEST));

            assert_eq!(
                raycast_hit,
                RaycastHit::HitTile(
                    Vec2::new(3, 0),
                    Dir::EAST,
                    &(
                        Vec2::new(3, 0),
                        Dir::EAST,
                        TileProgram::Machine(MachineInfo::BuiltIn(
                            BuiltInMachine::Produce(()),
                            ProgramInfo {},
                        ))
                    )
                )
            );
        }
        #[test]
        fn test_ray_hit_machine_2() {
            let prog = in_out_id_with_indirection_prog();

            let raycast_hit = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(6, 0), Dir::WEST));

            assert_eq!(
                raycast_hit,
                RaycastHit::HitTile(
                    Vec2::new(3, 0),
                    Dir::EAST,
                    &(
                        Vec2::new(3, 0),
                        Dir::EAST,
                        TileProgram::Machine(MachineInfo::BuiltIn(
                            BuiltInMachine::Produce(()),
                            ProgramInfo {},
                        ))
                    )
                )
            );
        }

        #[test]
        fn test_ray_hit_machine_other_way() {
            let prog = in_out_id_with_indirection_prog();

            let raycast_hit = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(0, 0), Dir::EAST));

            assert_eq!(
                raycast_hit,
                RaycastHit::HitTile(
                    Vec2::new(3, 0),
                    Dir::WEST,
                    &(
                        Vec2::new(3, 0),
                        Dir::EAST,
                        TileProgram::Machine(MachineInfo::BuiltIn(
                            BuiltInMachine::Produce(()),
                            ProgramInfo {},
                        ))
                    )
                )
            );
        }
        #[test]
        fn test_ray_hit_machine_other_way_2() {
            let prog = in_out_id_with_indirection_prog();

            let raycast_hit = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(1, 0), Dir::EAST));

            assert_eq!(
                raycast_hit,
                RaycastHit::HitTile(
                    Vec2::new(3, 0),
                    Dir::WEST,
                    &(
                        Vec2::new(3, 0),
                        Dir::EAST,
                        TileProgram::Machine(MachineInfo::BuiltIn(
                            BuiltInMachine::Produce(()),
                            ProgramInfo {},
                        ))
                    )
                )
            );
        }

        #[test]
        fn ray_doesnt_hit_when_shot_from_inside() {
            let prog = in_out_id_with_indirection_prog();

            let raycast_hit = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(3, 0), Dir::EAST));

            match raycast_hit {
                RaycastHit::HitBorder(_) => {}
                RaycastHit::HitTile(_, _, _) => {
                    panic!()
                }
            }
        }

        #[test]
        fn ray_east_edge_value_correct() {
            let prog = empty_program();

            let raycast_hit_east = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(2, 0), Dir::EAST));

            assert_eq!(
                raycast_hit_east,
                RaycastHit::HitBorder(GridLineDir::new(
                    Vec2::new(prog.program_dim().w, 0),
                    Dir::WEST
                ))
            )
        }
        #[test]
        fn ray_west_edge_value_correct() {
            let prog = empty_program();

            let raycast_hit_west = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(2, 0), Dir::WEST));

            assert_eq!(
                raycast_hit_west,
                RaycastHit::HitBorder(GridLineDir::new(Vec2i::new(-1, 0), Dir::EAST))
            )
        }

        #[test]
        fn get_output_grid_line_dir_faces_inwards() {
            let prog = empty_program();

            let dir = prog.get_output_grid_line_dir(0);

            assert_eq!(
                dir,
                GridLineDir::new(Vec2::new(prog.program_dim().w, 0), Dir::WEST)
            )
        }
        #[test]
        fn get_input_grid_line_dir_faces_inwards() {
            let prog = empty_program();

            let dir = prog.get_input_grid_line_dir(0);

            assert_eq!(dir, GridLineDir::new(Vec2i::new(-1, 0), Dir::EAST))
        }

        #[test]
        fn raycast_hit_equals_input() {
            let prog = empty_program();

            let raycast_hit_input = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(2, 0), Dir::WEST));

            assert_eq!(
                raycast_hit_input.to_normal(),
                prog.get_input_grid_line_dir(0)
            );
        }
        #[test]
        fn raycast_hit_equals_output() {
            let prog = empty_program();

            let raycast_hit_output = prog
                .spec
                .raycast(GridLineDir::new(Vec2::new(2, 0), Dir::EAST));

            match raycast_hit_output {
                RaycastHit::HitBorder(normal) => {
                    assert_eq!(normal, prog.get_output_grid_line_dir(0));
                }
                RaycastHit::HitTile(_, _, _) => {
                    panic!("shouldn't hit a tile on an empty map! duh!")
                }
            }
        }
        #[test]
        fn output_next_in_bounds() {
            let prog = empty_program();

            let loc = prog.get_output_grid_line_dir(0).next().0;

            match prog.spec.check_in_bounds_i(loc) {
                Some(_) => {}
                None => {
                    panic!("{} should be in bounds", loc)
                }
            }
        }
        #[test]
        fn output_prev_not_in_bounds() {
            let prog = empty_program();

            let loc = prog.get_output_grid_line_dir(0).previous().0;

            match prog.spec.check_in_bounds_i(loc) {
                Some(_) => {
                    panic!("{} shouldn't be in bounds", loc)
                }
                None => {}
            }
        }
    }

    #[cfg(test)]
    mod input_output {
        use pretty_assertions::assert_eq;

        use super::*;
        use crate::evaluation;
        use velcro::hash_map;

        #[test]
        fn test_id_program() {
            let data = WhnfData::Number(0);
            let prog = in_out_id_prog();

            let (graph, outputs) = evaluation::program_to_graph(&prog);

            let output_node = GraphNode::Output(outputs.into_iter().next().unwrap());
            let (output_data, _lasers_produced) = evaluation::weak_head_normal_form(
                &graph,
                &prog,
                Data::ThunkPure(output_node, Dependency::Only),
                vec![hash_map! {
                    prog.inputs[0].0:  Data::Whnf(data.clone())
                }],
            );
            assert_eq!(output_data, data);
        }

        #[test]
        fn test_id_program_with_indirection() {
            let data = WhnfData::Number(0);
            let prog = in_out_id_with_indirection_prog();

            let (graph, outputs) = evaluation::program_to_graph(&prog);

            let output_node = GraphNode::Output(outputs.into_iter().next().unwrap());
            let (output_data, _lasers_produced) = evaluation::weak_head_normal_form(
                &graph,
                &prog,
                Data::ThunkPure(output_node, Dependency::Only),
                vec![hash_map! {
                    prog.inputs[0].0:  Data::Whnf(data.clone())
                }],
            );
            assert_eq!(output_data, data);
        }

        #[test]
        fn test_get_value_from_literal() {
            let data = WhnfData::Number(0);
            let prog = const_prog();

            let (graph, outputs) = evaluation::program_to_graph(&prog);

            let output_node = GraphNode::Output(outputs.into_iter().next().unwrap());
            let (output_data, _lasers_produced) = evaluation::weak_head_normal_form(
                &graph,
                &prog,
                Data::ThunkPure(output_node, Dependency::Only),
                vec![hash_map! {
                    prog.inputs[0].0:  Data::Whnf(data.clone())
                }],
            );
            assert_eq!(output_data, WhnfData::Number(10));
        }
        #[test]
        fn test_literal_doesnt_work_at_distance() {
            let mut prog = in_out_id_prog();
            let data = prog.constants.insert(NfData::Number(10));
            let prog = prog
                .try_do_to_map(|map| {
                    let map = map.add(
                        Vec2::new(3, 0),
                        Dir::EAST,
                        TileProgram::Machine(MachineInfo::BuiltIn(
                            BuiltInMachine::Produce(()),
                            ProgramInfo {},
                        )),
                    )?;
                    map.add(Vec2::new(1, 0), Dir::NORTH, TileProgram::Literal(data))
                })
                .unwrap();

            let (graph, outputs) = evaluation::program_to_graph(&prog);

            let data = WhnfData::Number(0);

            let output_node = GraphNode::Output(outputs.into_iter().next().unwrap());
            let (output_data, _lasers_produced) = evaluation::weak_head_normal_form(
                &graph,
                &prog,
                Data::ThunkPure(output_node, Dependency::Only),
                vec![hash_map! {
                    prog.inputs[0].0:  Data::Whnf(data.clone())
                }],
            );
            println!("{}", evaluation::get_graph_str(&graph));
            assert_eq!(output_data, WhnfData::Nothing);
        }
    }
}
