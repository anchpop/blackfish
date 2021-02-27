use crate::types::*;
use frunk::monoid::Monoid;

pub fn sim(prog: TilemapProgram) -> TilemapWorld {
    simulate_until_stable(prog.into_world())
}

fn simulate_until_stable(mut world: TilemapWorld) -> TilemapWorld {
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

    let shape = new_world.world_dim();

    let mut propagate_lasers = |x: usize, y: usize| {
        for dir in [Dir::North, Dir::South, Dir::East, Dir::West].iter() {
            let dir_v = dir.to_vector();
            // the subtraction here should behave correctly,
            // see https://stackoverflow.com/questions/53453628/how-do-i-add-a-signed-integer-to-an-unsigned-integer-in-rust
            // and https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1448b2d8f02f844f72864e10dbe98049
            if let Some(adjacent) = world.get((
                x.wrapping_sub((dir_v.x as i64) as usize),
                y.wrapping_sub((dir_v.y as i64) as usize),
            )) {
                match adjacent {
                    TileWorld::Prog(TileProgramMachineInfo::LaserProducer(laser_dir, data)) => {
                        if dir == laser_dir {
                            new_world.add_laser(
                                (x, y),
                                DirData::empty().update(dir, Some(data.clone())),
                            );
                        }
                    }

                    TileWorld::Phys(TilePhysics::Laser(dir_data)) => {
                        if let Some(data) = dir_data.get(dir) {
                            new_world.add_laser(
                                (x, y),
                                DirData::empty().update(dir, Some(data.clone())),
                            );
                        }
                    }

                    _ => {}
                }
            }
        }
    };

    //let mut handle_receivers = |x: usize, y: usize| {};

    for x in 0..shape.0 {
        for y in 0..shape.1 {
            propagate_lasers(x, y);
            //handle_receivers(x, y);
        }
    }

    new_world
}
