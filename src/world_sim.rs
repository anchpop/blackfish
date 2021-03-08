use crate::types::*;
use bevy::ecs::Location;
use frunk::monoid::Monoid;

pub fn sim(prog: TilemapProgram) -> TilemapWorld {
    simulate_until_stable(prog.into_world())
}

pub fn simulate_until_stable(mut world: TilemapWorld) -> TilemapWorld {
    let mut count = 0;
    loop {
        if count > 4096 {
            panic!("Unable to get the tilemap stable after 4096 repetitions");
        }
        count += 1;

        let old_world = world.clone();
        world = iterate(world);
        if world == old_world {
            break world;
        }
    }
}

fn iterate(world: TilemapWorld) -> TilemapWorld {
    let mut new_world = world.clone();

    let propagate_lasers = |_tile: Option<&TileWorld>, location: XYPair| -> Option<Edit> {
        Some(Edit::Edits(
            [Dir::North, Dir::South, Dir::East, Dir::West]
                .iter()
                .map(|direction| {
                    if let Some(input) = world.get_input_to_coordinate(location, *direction) {
                        Some(Edit::AddLaser(
                            location,
                            DirMap::empty().update(direction, Some(input)),
                        ))
                    } else {
                        None
                    }
                })
                .flatten()
                .collect(),
        ))
    };

    let handle_machines = |tile: Option<&TileWorld>, location: XYPair| -> Option<Edit> {
        if let Some(TileWorld::Prog(TileProgramMachineInfo::Machine(
            orientation,
            MachineInfo::BuiltIn(builtin_type, info),
        ))) = tile
        {
            match builtin_type {
                BuiltInMachines::Iffy => {
                    todo!()
                }
                BuiltInMachines::Trace => Some(Edit::SetTile(
                    location,
                    TileWorld::Prog(TileProgramMachineInfo::Machine(
                        *orientation,
                        MachineInfo::BuiltIn(
                            *builtin_type,
                            WorldMachineInfo {
                                display: Some({
                                    if let Some(data) = world.get_input_to_coordinate(
                                        location,
                                        Dir::North.rotate(*orientation),
                                    ) {
                                        data.show()
                                    } else {
                                        "".to_string()
                                    }
                                }),
                                program_info: info.program_info.clone(),
                            },
                        ),
                    )),
                )),

                BuiltInMachines::Produce => {
                    // I don't actually think there's anything to do here
                    None
                }
            }
        } else {
            None
        }
    };

    let validate_stuff = |tile: Option<&TileWorld>, location: XYPair| -> Option<Edit> {
        if let Some(tile) = tile {
            match tile {
                TileWorld::Phys(_) => {}
                TileWorld::Prog(tile) => match tile {
                    TileProgramF::Machine(_, machine_info) => match machine_info {
                        MachineInfo::BuiltIn(machine_type, machine_info) => {
                            let inputs: Vec<String> = machine_type
                                .io()
                                .values()
                                .filter_map(|io| {
                                    if let IOType::In(s) = io {
                                        Some(s)
                                    } else {
                                        None
                                    }
                                })
                                .cloned()
                                .collect();
                            for k in machine_info.program_info.hardcoded_inputs.keys() {
                                assert!(inputs.contains(k))
                            }
                        }
                    },
                },
            }
        };
        None
    };

    new_world.apply_transformation(&world, validate_stuff);
    new_world.apply_transformation(&world, propagate_lasers);
    new_world.apply_transformation(&world, handle_machines);

    new_world
}
