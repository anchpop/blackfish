use crate::types::*;

use frunk::monoid::Monoid;
use frunk::semigroup::Semigroup;

use ndarray::arr2;

use velcro::btree_map;
use velcro::hash_map;

fn default_map() -> TilemapProgram {
    let mut tiles = TilemapProgram::make_slotmap();
    let north_laser = tiles.insert(TileProgram::Machine(
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
    })
}

fn empty_map() -> TilemapProgram {
    let tiles = TilemapProgram::make_slotmap();

    TilemapProgram::new(Tilemap {
        tiles,
        map: arr2(&[
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
        ]),
    })
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

    // forall pos tilemap tile. { tilemap.set_tile(pos, tile); tilemap.get(pos) == tile }

    #[cfg(test)]
    mod equality {
        use super::*;

        #[test]
        fn basic_tilemap_equality() {
            assert_eq!(default_map(), default_map());
        }

        #[test]
        fn tilemap_world_equality() {
            assert_eq!(default_map().into_world(), default_map().into_world());
        }

        #[test]
        fn tilemap_equality_invariant_to_unreferenced_tiles() {
            let m1 = default_map();
            let mut m2 = m1.clone();
            m2.spec.tiles.insert(TileProgram::Machine(
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
            assert_eq!(m1, m2);
        }

        #[test]
        fn tilemap_equality_alpha_equivalence() {
            let m1 = default_map();
            let mut m2 = m1.clone();
            m2.spec.set_tile(
                (3, 2),
                TileProgram::Machine(
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
                ),
            );
            assert_eq!(m1, m2);
        }

        #[test]
        fn tilemap_inequality() {
            let m1 = default_map();
            let mut m2 = m1.clone();
            m2.spec.set_tile(
                (0, 0),
                TileProgram::Machine(
                    Dir::North,
                    MachineInfo::BuiltIn(
                        BuiltInMachines::Produce,
                        ProgramInfo {
                            hardcoded_inputs: btree_map! {
                                "product".to_string(): Data::Number(3)
                            },
                            ..ProgramInfo::empty()
                        },
                    ),
                ),
            );
            assert_ne!(m1, m2);
        }
    }

    #[cfg(test)]
    mod input_output {
        use std::collections::BTreeMap;

        use frunk::Monoid;

        use super::*;
        use crate::world_sim;

        #[test]
        fn get_inputs_to_coordinate() {
            let data = Data::Number(2);
            let map = {
                let mut map = empty_map();
                map.add_tile(
                    (3, 1),
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

            let world = world_sim::sim(map, hash_map! {});
            assert_eq!(
                world.get_input_to_coordinate((3, 3), Dir::North).unwrap(),
                data
            );
        }

        #[test]
        fn get_input_hardcoded() {
            let data = Data::Number(2);
            let hardcoded_inputs = btree_map! {
                "product".to_string(): data.clone()
            };
            let location = (3, 1);

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
                map.into_world().get_inputs(location),
                Some(hardcoded_inputs)
            );
        }

        #[test]
        fn test_no_inputs() {
            let hardcoded_inputs = btree_map! {};
            let location = (3, 1);

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

            assert_eq!(map.into_world().get_inputs(location), None);
        }

        #[test]
        fn test_passed_inputs() {
            let data = Data::Number(2);
            let passed_inputs = btree_map! {
                "product".to_string(): data.clone()
            };
            let location = (3, 3);

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
                    (location.0, location.1 - 2),
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
                world_sim::sim(map, hash_map! {}).get_inputs(location),
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
            let location = (3, 3);

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
                    (location.0, location.1 - 1),
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

            world_sim::sim(map, hash_map! {}).get_inputs(location);
        }
    }
}
