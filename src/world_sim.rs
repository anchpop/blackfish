use crate::types::*;

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
    let world = world.world;
    let mut new_world = world.clone();

    let shape = new_world.dim();
    for y in 0..shape.0 {
        for x in 0..shape.1 {
            if let Some(below) = world.get([y.wrapping_sub(1), x]) {
                if let None = world[[y, x]] {
                    match below {
                        Some(
                            TileWorld::Prog(TileProgram::LaserProducer(_))
                            | TileWorld::Phys(TilePhysics::Laser),
                        ) => new_world[[y, x]] = Some(TileWorld::Phys(TilePhysics::Laser)),
                        _ => {}
                    }
                }
            }
        }
    }

    TilemapWorld { world: new_world }
}
