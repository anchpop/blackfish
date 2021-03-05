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
                    if let Some(input) = world.get_input(location, *direction) {
                        Some(Edit::AddLaser(
                            location,
                            DirMap::empty().update(direction, Some(input.clone())),
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
            MachineInfo::BuiltIn(builtin_type, _),
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
                                    if let Some(data) =
                                        world.get_input(location, Dir::North.rotate(*orientation))
                                    {
                                        data.show()
                                    } else {
                                        "".to_string()
                                    }
                                }),
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

    new_world.apply_transformation(&world, propagate_lasers);
    new_world.apply_transformation(&world, handle_machines);

    new_world
}
