use crate::types::*;
use frunk::monoid::Monoid;

pub fn sim(prog: TilemapProgram) -> TilemapWorld {
    let world = TilemapWorld {
        world: prog.program.mapv(|a| a.map(TileWorld::Prog)),
    };
    simulate_until_stable(world)
}

fn simulate_until_stable(mut world: TilemapWorld) -> TilemapWorld {
    loop {
        let old_world = world.clone();
        world = iterate(world);
        if world == old_world {
            return world;
        }
    }
}

fn iterate(world: TilemapWorld) -> TilemapWorld {
    let mut new_world = world.clone();

    let shape = new_world.world.dim();
    for y in 0..shape.0 {
        for x in 0..shape.1 {
            for dir in [Dir::North, Dir::South, Dir::East, Dir::West].iter() {
                let dir_v = dir.to_vector();
                // addition here should behave correctly,
                // see https://stackoverflow.com/questions/53453628/how-do-i-add-a-signed-integer-to-an-unsigned-integer-in-rust
                // and https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1448b2d8f02f844f72864e10dbe98049
                if let Some(adjacent) = world.get((
                    x.wrapping_sub((dir_v.x as i64) as usize),
                    y.wrapping_sub((dir_v.y as i64) as usize),
                )) {
                    match adjacent {
                        Some(TileWorld::Prog(TileProgram::LaserProducer(laser_dir, data))) => {
                            if dir == laser_dir {
                                new_world.add_laser(
                                    (x, y),
                                    DirData::empty().update(dir, Some(data.clone())),
                                );
                            }
                        }

                        Some(TileWorld::Phys(TilePhysics::Laser(dir_data))) => {
                            if let Some(data) = dir_data.get(dir) {
                                new_world.add_laser(
                                    (x, y),
                                    DirData::empty().update(dir, Some(data.clone())),
                                );
                            }
                        }

                        _ => {}
                    }
                    /*
                    match adjacent {
                        Some(
                            TileWorld::Prog(TileProgram::LaserProducer(_, data))
                            | TileWorld::Phys(TilePhysics::Laser),
                        ) => new_world[[y, x]] = Some(TileWorld::Phys(TilePhysics::Laser)),
                        _ => {}
                    }*/
                }
            }
        }
    }

    new_world
}
