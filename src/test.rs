use crate::geom::direction::*;
use crate::geom::Extent2;
use crate::geom::Vec2;
use crate::geom::Vec2i;
use crate::types::data::*;
use crate::types::tilemaps::*;
use crate::types::tiles::*;
use crate::types::*;

use frunk::monoid::Monoid;
use frunk::semigroup::Semigroup;

use ndarray::arr2;

use velcro::btree_map;
use velcro::hash_map;

use crate::evaluation;

pub fn default_program() -> TilemapProgram {
    in_out_id_with_indirection_prog()
}

pub fn in_out_id_with_indirection_prog() -> TilemapProgram {
    in_out_id_prog()
        .try_do_to_map(|map| {
            map.add(
                Vec2::new(3, 0),
                Dir::default(),
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {
                        hardcoded_inputs: btree_map! {
                            "product".to_string(): Data::Number(3)
                        },
                        ..ProgramInfo::empty()
                    },
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
                ProgramInfo {
                    hardcoded_inputs: btree_map! {
                        "product".to_string(): Data::Number(3)
                    },
                    ..ProgramInfo::empty()
                },
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

        #[test]
        fn basic_tilemap_equality_empty() {
            assert_eq!(empty_program(), empty_program());
        }
        #[test]
        fn basic_tilemap_equality_simple() {
            assert_eq!(simple_program(), simple_program());
        }
        #[test]
        fn basic_tilemap_inequality_default() {
            // the default programs should have different UUIDs making them different
            assert_ne!(default_program(), default_program());
        }
        #[test]
        fn basic_tilemap_inequality_default_2() {
            assert_ne!(default_program(), empty_program());
        }

        #[test]
        fn tilemap_world_equality() {
            assert_eq!(
                default_program().into_world(vec![], vec![]),
                default_program().into_world(vec![], vec![])
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
                    ProgramInfo {
                        hardcoded_inputs: btree_map! {
                            "product".to_string(): Data::Number(3)
                        },
                        ..ProgramInfo::empty()
                    },
                )),
            ));
            assert_eq!(m1, m2);
        }
    }

    #[cfg(test)]
    mod gridlines {
        use std::collections::BTreeMap;

        use frunk::Monoid;

        use super::*;
        use crate::evaluation;

        #[test]
        fn grid_line_east() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-1, 0),
                    side: Basis::East
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::east)
            );
        }
        #[test]
        fn grid_line_north() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-1, 0),
                    side: Basis::North
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::north)
            );
        }
        #[test]
        fn grid_line_south() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-1, -1),
                    side: Basis::North
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::south)
            );
        }
        #[test]
        fn grid_line_west() {
            assert_eq!(
                GridLine {
                    location: Vec2i::new(-2, 0),
                    side: Basis::East
                },
                GridLine::new(Vec2i::new(-1, 0), Dir::west)
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
                GridLineDir::new(Vec2i::new(-1, 0), Dir::east)
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
                GridLineDir::new(Vec2i::new(-1, 0), Dir::north)
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
                GridLineDir::new(Vec2i::new(-1, 0), Dir::west)
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
                GridLineDir::new(Vec2i::new(-1, 0), Dir::south)
            );
        }

        #[test]
        fn grid_line_dir_parts_north() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::north;

            let (recovered_start, recovered_dir) = GridLineDir::new(start, dir).parts();
            assert_eq!(start, recovered_start);
        }
        #[test]
        fn grid_line_dir_parts_south() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::south;

            let (recovered_start, recovered_dir) = GridLineDir::new(start, dir).parts();
            assert_eq!(start, recovered_start);
        }
        #[test]
        fn grid_line_dir_parts_east() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::east;

            let (recovered_start, recovered_dir) = GridLineDir::new(start, dir).parts();
            assert_eq!(start, recovered_start);
        }
        #[test]
        fn grid_line_dir_parts_west() {
            let start = Vec2i::new(-1, 0);
            let dir = Dir::west;

            let (recovered_start, recovered_dir) = GridLineDir::new(start, dir).parts();
            assert_eq!(start, recovered_start);
        }
    }
    /*
       #[cfg(test)]
       mod raycast {
           use std::collections::BTreeMap;

           use frunk::Monoid;

           use super::*;
           use crate::evaluation;

           #[test]
           fn test_ray_hit_edge() {
               let prog = in_out_id_prog();

               let (raycast_hit_gridline, raycast_hit) = prog
                   .spec
                   .raycast(GridLineDir::new(Vec2::new(5, 0), Dir::west));

               assert_eq!(raycast_hit, None);
               assert_eq!(
                   raycast_hit_gridline,
                   GridLineDir::new(Vec2i::new(-1, 0), Dir::east)
               );
           }

           #[test]
           fn test_ray_hit_machine() {
               let prog = in_out_id_with_indirection_prog();

               let (raycast_hit_gridline, raycast_hit) = prog
                   .spec
                   .raycast(GridLineDir::new(Vec2::new(5, 0), Dir::west));

               assert!(raycast_hit.is_some());
               assert_eq!(
                   raycast_hit_gridline,
                   GridLineDir::new(Vec2i::new(3, 0), Dir::east)
               );
           }
           #[test]
           fn test_ray_hit_machine_2() {
               let prog = in_out_id_with_indirection_prog();

               let (raycast_hit_gridline, raycast_hit) = prog
                   .spec
                   .raycast(GridLineDir::new(Vec2::new(6, 0), Dir::west));

               assert!(raycast_hit.is_some());
               assert_eq!(
                   raycast_hit_gridline,
                   GridLineDir::new(Vec2i::new(3, 0), Dir::east)
               );
           }

           #[test]
           fn test_ray_hit_machine_other_way() {
               let prog = in_out_id_with_indirection_prog();

               let (raycast_hit_gridline, raycast_hit) = prog
                   .spec
                   .raycast(GridLineDir::new(Vec2::new(0, 0), Dir::east));

               assert!(raycast_hit.is_some());
               assert_eq!(
                   raycast_hit_gridline,
                   GridLineDir::new(Vec2i::new(3, 0), Dir::west)
               );
           }
           #[test]
           fn test_ray_hit_machine_other_way_2() {
               let prog = in_out_id_with_indirection_prog();

               let (raycast_hit_gridline, raycast_hit) = prog
                   .spec
                   .raycast(GridLineDir::new(Vec2::new(1, 0), Dir::east));

               assert!(raycast_hit.is_some());
               assert_eq!(
                   raycast_hit_gridline,
                   GridLineDir::new(Vec2i::new(3, 0), Dir::west)
               );
           }
       }
    */
    #[cfg(test)]
    mod input_output {
        use std::collections::BTreeMap;

        use frunk::Monoid;

        use super::*;
        use crate::evaluation;

        #[test]
        fn test_id_program() {
            let data = Data::Number(0);
            let prog = in_out_id_prog();

            let input_uuid = prog.inputs[0].0;
            let output_uuid = prog.outputs[0].0;

            let result = evaluation::evaluate(
                prog.clone(),
                std::array::IntoIter::new([(input_uuid, data.clone())]).collect(),
            );
            assert_eq!(result.1.get(&output_uuid).unwrap(), &data);
        }

        #[test]
        fn test_id_program_with_indirection() {
            let data = Data::Number(0);
            let prog = in_out_id_with_indirection_prog();

            let input_uuid = prog.inputs[0].0;
            let output_uuid = prog.outputs[0].0;

            let result = evaluation::evaluate(
                prog.clone(),
                std::array::IntoIter::new([(input_uuid, data.clone())]).collect(),
            );
            assert_eq!(result.1.get(&output_uuid).unwrap(), &data);
        }

        /*

               #[test]
               fn get_inputs_to_coordinate() {
                   let data = Data::Number(2);
                   let map = {
                       let mut map = empty_map();
                       map.add_tile(
                           Vec2::new(3, 1),
                           TileProgram::Machine(
                               Dir::North,
                               MachineInfo::BuiltIn(
                                   BuiltInMachines::Produce,
                                   ProgramInfo {
                                       hardcoded_inputs: btree_map! {
                                           "product".to_string(): data.clone()
                                       },
                                       ..ProgramInfo::empty()
                                   },
                               ),
                           ),
                       );
                       map
                   };

                   let world = evaluation::evaluate(map, hash_map! {}).0;
                   assert_eq!(
                       world
                           .get_input_to_coordinate(Vec2::new(3, 3), Dir::North)
                           .unwrap(),
                       data
                   );
               }

               #[test]
               fn get_input_hardcoded() {
                   let data = Data::Number(2);
                   let hardcoded_inputs = btree_map! {
                       "product".to_string(): data.clone()
                   };
                   let location = Vec2::new(3, 1);

                   let map = {
                       let mut map = empty_map();
                       map.add_tile(
                           location,
                           TileProgram::Machine(
                               Dir::North,
                               MachineInfo::BuiltIn(
                                   BuiltInMachines::Produce,
                                   ProgramInfo {
                                       hardcoded_inputs: hardcoded_inputs.clone(),
                                       ..ProgramInfo::empty()
                                   },
                               ),
                           ),
                       );
                       map
                   };

                   assert_eq!(
                       map.into_world(hash_map! {}).get_inputs(location),
                       Some(hardcoded_inputs)
                   );
               }

               #[test]
               fn test_no_inputs() {
                   let hardcoded_inputs = btree_map! {};
                   let location = Vec2::new(3, 1);

                   let map = {
                       let mut map = empty_map();
                       map.add_tile(
                           location,
                           TileProgram::Machine(
                               Dir::North,
                               MachineInfo::BuiltIn(
                                   BuiltInMachines::Produce,
                                   ProgramInfo {
                                       hardcoded_inputs: hardcoded_inputs.clone(),
                                       ..ProgramInfo::empty()
                                   },
                               ),
                           ),
                       );
                       map
                   };

                   assert_eq!(map.into_world(hash_map! {}).get_inputs(location), None);
               }

               #[test]
               fn test_passed_inputs() {
                   let data = Data::Number(2);
                   let passed_inputs = btree_map! {
                       "product".to_string(): data.clone()
                   };
                   let location = Vec2::new(3, 3);

                   let map = {
                       let mut map = empty_map();
                       map.add_tile(
                           location,
                           TileProgram::Machine(
                               Dir::North,
                               MachineInfo::BuiltIn(
                                   BuiltInMachines::Produce,
                                   ProgramInfo {
                                       hardcoded_inputs: btree_map! {},
                                       ..ProgramInfo::empty()
                                   },
                               ),
                           ),
                       );
                       map.add_tile(
                           location - Vec2::new(0, 2),
                           TileProgram::Machine(
                               Dir::default(),
                               MachineInfo::BuiltIn(
                                   BuiltInMachines::Produce,
                                   ProgramInfo {
                                       hardcoded_inputs: passed_inputs.clone(),
                                   },
                               ),
                           ),
                       );
                       map
                   };

                   assert_eq!(
                       evaluation::evaluate(map, hash_map! {})
                           .0
                           .get_inputs(location),
                       Some(passed_inputs)
                   );
               }

               // this causes an infinite loop even though it doesn't strictly need to in the current setup.
               // get_input_to_coordinate just needs to check the contents of the tile it's accessing, and
               // only actually attempt to compute it if it can possibly output anything in the direction
               // that's being asked about
               // However, some infinite loops are inevitable, and since I'm eventaully going to rearch
               // everything anyway I don't feel like bothering to fix this when it'll just have to be
               // redone once I switch to lazy evaluation (or pull-based instead of push-based).
               #[test]
               #[ignore]
               fn unecessary_infinite_loop() {
                   let data = Data::Number(2);
                   let location = Vec2::new(3, 3);

                   let map = {
                       let mut map = empty_map();
                       map.add_tile(
                           location,
                           TileProgram::Machine(
                               Dir::North,
                               MachineInfo::BuiltIn(
                                   BuiltInMachines::Produce,
                                   ProgramInfo {
                                       hardcoded_inputs: btree_map! {},
                                       ..ProgramInfo::empty()
                                   },
                               ),
                           ),
                       );
                       map.add_tile(
                           location - Vec2::new(0, 1),
                           TileProgram::Machine(
                               Dir::South,
                               MachineInfo::BuiltIn(
                                   BuiltInMachines::Produce,
                                   ProgramInfo {
                                       hardcoded_inputs: btree_map! {},
                                       ..ProgramInfo::empty()
                                   },
                               ),
                           ),
                       );
                       map
                   };

                   evaluation::evaluate(map, hash_map! {})
                       .0
                       .get_inputs(location);
               }
        */
    }
}
