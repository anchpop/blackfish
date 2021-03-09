mod test;
mod types;
mod world_sim;

use types::*;
use world_sim::{sim, simulate_until_stable};

use bevy::prelude::*;

use ndarray::arr2;
use slotmap::{new_key_type, Key, SlotMap};

use frunk::monoid::Monoid;

use std::time::Duration;

use velcro::btree_map;

#[derive(Debug, Clone)]
struct ClockIncrementTimer(Timer);
impl Default for ClockIncrementTimer {
    fn default() -> Self {
        Self(Timer::new(Duration::from_millis(1000), true))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct CurrentClock(i32);

fn main() {
    App::build()
        .add_startup_system(setup.system())
        .add_startup_system(create_map.system())
        .add_startup_stage("game_setup", SystemStage::single(spawn_main_tile.system())) // Adding a stage lets us access resources (in this case, materials) created in the previous stage
        .add_plugins(DefaultPlugins)
        .add_system(clock_increment.system())
        .add_system(size_scaling.system())
        .add_system(positioning.system())
        .add_system(tile_appearance.system())
        .add_system(tile_text.system())
        .add_system(update_master_input.system())
        .run();
}

fn setup(commands: &mut Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.insert_resource(CurrentClock(0));

    commands.spawn(OrthographicCameraBundle::new_2d());
    commands.insert_resource(Materials {
        empty: materials.add(Color::rgb(0.1, 0.1, 0.1).into()),
        tiles: [
            (
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
                )
                .name(),
                materials.add(Color::rgb(0.3, 0.3, 0.3).into()),
            ),
            (
                TileWorld::Phys(TilePhysics::Laser(DirMap::empty())).name(),
                materials.add(Color::rgb(0.9, 0.3, 0.3).into()),
            ),
            (
                TileProgram::Machine(
                    Dir::default(),
                    MachineInfo::BuiltIn(
                        BuiltInMachines::Trace,
                        ProgramInfo {
                            hardcoded_inputs: btree_map! {
                                "product".to_string(): Data::Number(3)
                            },
                            ..ProgramInfo::empty()
                        },
                    ),
                )
                .name(),
                materials.add(Color::rgb(0.5, 0.3, 0.5).into()),
            ),
            (
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
                )
                .name(),
                materials.add(Color::rgb(0.3, 0.3, 0.3).into()),
            ),
        ]
        .iter()
        .cloned()
        .collect(),
    });
}

fn create_map(commands: &mut Commands) {
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
        Dir::West,
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
        Dir::West,
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
    let laser_machine_test = tiles.insert(TileProgram::Machine(
        Dir::East,
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
    let tracer = tiles.insert(TileProgram::Machine(
        Dir::East,
        MachineInfo::BuiltIn(BuiltInMachines::Trace, ProgramInfo::empty()),
    ));
    let tracer2 = tiles.insert(TileProgram::Machine(
        Dir::West,
        MachineInfo::BuiltIn(BuiltInMachines::Trace, ProgramInfo::empty()),
    ));

    let test_prog = TilemapProgram(Tilemap {
        tiles,
        map: arr2(&[
            [None, None, None, None, None, None, None, None, Some(tracer)],
            [
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(laser_machine_test),
                None,
            ],
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
                Some(tracer2),
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
    });

    let test_world = sim(test_prog.clone());

    commands.insert_resource(test_prog);
    commands.insert_resource(test_world);
}

fn spawn_main_tile(
    commands: &mut Commands,
    tilemap: Res<TilemapWorld>,
    asset_server: Res<AssetServer>,
) {
    enum TileType {
        Real,
        Borderland,
    }
    for location in tilemap
        .0
        .map
        .indexed_iter()
        .map(|(index, _)| (TileType::Real, (index.1, index.0)))
        .chain((0..tilemap.0.map.dim().1).map(|x| (TileType::Borderland, (x, 0))))
        .chain(
            (0..tilemap.0.map.dim().1)
                .map(|x| (TileType::Borderland, (x, tilemap.0.map.dim().0 - 1))),
        )
        .chain((0..tilemap.0.map.dim().0).map(|y| (TileType::Borderland, (0, y))))
        .chain(
            (0..tilemap.0.map.dim().0)
                .map(|y| (TileType::Borderland, (tilemap.0.map.dim().1 - 1, y))),
        )
    {
        if let (TileType::Real, location) = location {
            let pos = TilePosition {
                x: location.0,
                y: location.1,
            };
            let size = TileSize {
                width: 1,
                height: 1,
            };
            commands
                .spawn(SpriteBundle {
                    sprite: Sprite::new(Vec2::new(10., 10.)),
                    ..Default::default()
                })
                .with(pos)
                .with(size)
                .with_children(|parent| {
                    parent.spawn(Text2dBundle {
                        text: Text::with_section(
                            format!("{}, {}", location.0, location.1),
                            TextStyle {
                                font: asset_server.load("fonts/FiraSans/FiraSans-Light.ttf"),
                                font_size: 60.0,
                                color: Color::WHITE,
                            },
                            TextAlignment {
                                vertical: VerticalAlign::Center,
                                horizontal: HorizontalAlign::Center,
                            },
                        ),
                        transform: Transform {
                            translation: Vec3::new(0., 0., 2.),
                            rotation: Quat::identity(),
                            scale: Vec3::new(0., 0., 0.),
                        },
                        ..Default::default()
                    });
                });
        }
    }
}

fn update_master_input(
    clock: Res<CurrentClock>,
    mut tilemap_world: ResMut<TilemapWorld>,
    tilemap_program: Res<TilemapProgram>,
) {
    let new_prog = tilemap_program.clone();
    let mut new_world = new_prog.into_world();
    new_world.set_tile(
        (0, 0),
        TileWorld::Prog(TileProgramMachineInfo::Machine(
            Dir::East,
            MachineInfo::BuiltIn(
                BuiltInMachines::Produce,
                WorldMachineInfo {
                    program_info: ProgramInfo {
                        hardcoded_inputs: btree_map! {
                            "product".to_string(): Data::Number(clock.0)
                        },
                        ..ProgramInfo::empty()
                    },
                    ..WorldMachineInfo::empty()
                },
            ),
        )),
    );
    *tilemap_world = simulate_until_stable(new_world);
}

fn size_scaling(
    windows: Res<Windows>,
    mut q: Query<(&TileSize, &mut Sprite)>,
    tilemap: Res<TilemapWorld>,
) {
    let (arena_tiles_wide, arena_tiles_tall) = tilemap.world_dim();
    let (arena_tiles_wide, arena_tiles_tall) = (arena_tiles_wide + 2, arena_tiles_tall + 2);
    let window = windows.get_primary().unwrap();

    for (sprite_size, mut sprite) in q.iter_mut() {
        sprite.size = Vec2::new(
            sprite_size.width as f32 / arena_tiles_wide as f32 * window.width() as f32,
            sprite_size.height as f32 / arena_tiles_tall as f32 * window.height() as f32,
        );
    }
}

fn positioning(
    windows: Res<Windows>,
    mut q: Query<(&TilePosition, &mut Transform)>,
    tilemap: Res<TilemapWorld>,
) {
    let (arena_tiles_wide, arena_tiles_tall) = tilemap.world_dim();
    let (arena_tiles_wide, arena_tiles_tall) = (arena_tiles_wide + 2, arena_tiles_tall + 2);
    let window = windows.get_primary().unwrap();

    fn convert(pos: usize, bound_window: f32, bound_game: f32) -> f32 {
        let tile_size = bound_window / bound_game;
        ((pos + 1) as f32) / bound_game * bound_window - (bound_window / 2.) + (tile_size / 2.)
    }

    for (pos, mut transform) in q.iter_mut() {
        transform.translation = Vec3::new(
            convert(pos.x, window.width() as f32, arena_tiles_wide as f32),
            convert(pos.y, window.height() as f32, arena_tiles_tall as f32),
            0.0,
        );
    }
}

fn get_tile_material(tile: &Option<&TileWorld>, materials: &Materials) -> Handle<ColorMaterial> {
    match tile {
        None => materials.empty.clone(),
        Some(t) => materials.tiles[t.name()].clone(),
    }
}

fn tile_appearance(
    mut q: Query<(&TilePosition, &mut Handle<ColorMaterial>)>,
    materials: Res<Materials>,
    tilemap: Res<TilemapWorld>,
) {
    for (tile_position, mut color_mat_handle) in q.iter_mut() {
        let tile = tilemap.get((tile_position.x, tile_position.y));
        *color_mat_handle = get_tile_material(&tile, &materials);
    }
}

fn tile_text(
    mut q: Query<(&TilePosition, &Children)>,
    mut text_q: Query<&mut Text>,
    tilemap: Res<TilemapWorld>,
) {
    for (tile_position, children) in q.iter_mut() {
        let tile = tilemap.get((tile_position.x, tile_position.y));
        for child in children.iter() {
            if let Ok(mut text) = text_q.get_mut(*child) {
                text.sections[0].value = match tile {
                    /*Some(TileWorld::Prog(TileProgramMachineInfo::LaserProducer(dir, data))) => {
                        format!("{} {}", dir.to_arrow(), data.show())
                    }*/
                    Some(TileWorld::Prog(TileProgramMachineInfo::Machine(
                        dir,
                        MachineInfo::BuiltIn(_, data),
                    ))) => {
                        if let Some(text) = data.display.clone() {
                            format!("{} {}", dir.to_arrow(), text)
                        } else {
                            dir.to_arrow().to_string()
                        }
                        //format!("{} {}", dir.to_arrow(), data.show())
                    }
                    _ => "".to_string(),
                };
            }
        }
    }
}

fn clock_increment(
    time: Res<Time>,
    mut timer: Local<ClockIncrementTimer>,
    mut clock: ResMut<CurrentClock>,
) {
    if timer.0.tick(time.delta_seconds()).finished() {
        *clock = CurrentClock(clock.0 + 1);
    }
}
