mod evaluation;
mod geom;
mod test;
mod types;
mod units;
#[macro_use]
extern crate uom;

use crate::geom::direction::*;
use evaluation::evaluate;
use geom::Extent2;
use geom::Vec2;
use types::data::*;
use types::tilemaps::*;
use types::tiles::*;
use types::*;

use bevy::prelude::*;

use ndarray::arr2;
use slotmap::{new_key_type, Key, SlotMap};

use frunk::monoid::Monoid;

use std::time::Duration;

use velcro::btree_map;
use velcro::hash_map;

use std::error::Error;
use std::io::{stdin, stdout, Write};
use std::thread::sleep;

use midir::{MidiOutput, MidiOutputConnection, MidiOutputPort};

use std::sync::{Mutex, MutexGuard};

use test::default_program;

use units::f64::*;
use units::music_time::{bang, beat, note};

const NOTE_ON_MSG: u8 = 0x90;
const NOTE_OFF_MSG: u8 = 0x80;
const VELOCITY: u8 = 0x64;

const BEATS_PER_SECOND: u64 = 1;

#[derive(Debug, Clone)]
struct ClockIncrementTimer(Timer);
impl Default for ClockIncrementTimer {
    fn default() -> Self {
        Self(Timer::new(Duration::from_millis(1000), true))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct CurrentClock(i32);

#[derive(Debug, Clone)]
struct NotesToEnd(Vec<(Timer, u8, u8)>);

fn main() {
    App::build()
        .add_startup_system(get_midi_ports.system())
        .add_startup_system(setup.system())
        .add_startup_system(create_map.system())
        // Adding a stage lets us access resources (in this case, materials) created in the previous stage
        .add_startup_stage("game_setup", SystemStage::single(spawn_main_tile.system()))
        /* .add_startup_stage(
            "play_notes_test",
            SystemStage::single(play_notes_test.system()),
        )*/
        .add_plugins(DefaultPlugins)
        .add_system(clock_increment.system())
        .add_system(end_started_notes.system())
        .add_system(size_scaling.system())
        .add_system(positioning.system())
        .add_system(tile_appearance.system())
        .add_system(tile_text.system())
        .run();
}

fn setup(commands: &mut Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.insert_resource(CurrentClock(0));

    commands.spawn(OrthographicCameraBundle::new_2d());
    commands.insert_resource(Materials {
        empty: materials.add(Color::rgb(0.1, 0.1, 0.1).into()),
        tiles: [
            (
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {
                        hardcoded_inputs: btree_map! {
                            "product".to_string(): Data::Number(3)
                        },
                        ..ProgramInfo::empty()
                    },
                ))
                .name(),
                materials.add(Color::rgb(0.3, 0.3, 0.3).into()),
            ),
            (
                TileWorld::Phys(TilePhysics::Laser(DirMap::empty())).name(),
                materials.add(Color::rgb(0.9, 0.3, 0.3).into()),
            ),
            (
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Trace(()),
                    ProgramInfo {
                        hardcoded_inputs: btree_map! {
                            "product".to_string(): Data::Number(3)
                        },
                        ..ProgramInfo::empty()
                    },
                ))
                .name(),
                materials.add(Color::rgb(0.5, 0.3, 0.5).into()),
            ),
            (
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {
                        hardcoded_inputs: btree_map! {
                            "product".to_string(): Data::Number(3)
                        },
                        ..ProgramInfo::empty()
                    },
                ))
                .name(),
                materials.add(Color::rgb(0.3, 0.3, 0.3).into()),
            ),
        ]
        .iter()
        .cloned()
        .collect(),
    });
}

fn get_midi_ports(commands: &mut Commands) {
    fn run(commands: &mut Commands) -> Result<(), Box<dyn Error>> {
        let midi_out = MidiOutput::new("Blackfish Midi Output")?;

        // Get an output port (read from console if multiple are available)
        let out_ports = midi_out.ports();
        let out_port: &MidiOutputPort = match out_ports.len() {
            0 => return Err("no output port found".into()),
            1 => {
                println!(
                    "Choosing the only available output port: {}",
                    midi_out.port_name(&out_ports[0]).unwrap()
                );
                &out_ports[0]
            }
            _ => {
                println!("\nAvailable output ports:");
                for (i, p) in out_ports.iter().enumerate() {
                    println!("{}: {}", i, midi_out.port_name(p).unwrap());
                }
                print!("Please select output port: ");
                stdout().flush()?;
                let mut input = String::new();
                stdin().read_line(&mut input)?;
                out_ports
                    .get(input.trim().parse::<usize>()?)
                    .ok_or("invalid output port selected")?
            }
        };
        println!("\nOpening connection");
        let conn_out = midi_out.connect(out_port, "blackfish-con-1")?;

        commands.insert_resource(Mutex::new(conn_out));

        Ok(())
    }

    commands.insert_resource(NotesToEnd(vec![]));

    match run(commands) {
        Ok(_) => (),
        Err(err) => println!("Error: {}", err),
    }
}

fn play_notes_test(conn_out: Res<Mutex<midir::MidiOutputConnection>>) {
    let mut conn_out = conn_out.lock().unwrap();
    println!("Connection open. Listen!");
    {
        // Define a new scope in which the closure `play_note` borrows conn_out, so it can be called easily
        let mut play_note = |pitch: u8, duration: u64| {
            const NOTE_ON_MSG: u8 = 0x90;
            const NOTE_OFF_MSG: u8 = 0x80;
            const VELOCITY: u8 = 0x64;
            // We're ignoring errors in here
            let _ = conn_out.send(&[NOTE_ON_MSG, pitch, VELOCITY]);
            sleep(Duration::from_millis(duration * 150));
            let _ = conn_out.send(&[NOTE_OFF_MSG, pitch, VELOCITY]);
        };

        sleep(Duration::from_millis(4 * 150));

        play_note(66, 4);
        play_note(65, 3);
        play_note(63, 1);
        play_note(61, 6);
        play_note(59, 2);
        play_note(58, 4);
        play_note(56, 4);
        play_note(54, 4);
    }
}

fn create_map(commands: &mut Commands) {
    let test_prog = default_program();

    /*
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

    let id_laser_test = tiles.insert(TileProgram::Machine(
        Dir::East,
        MachineInfo::BuiltIn(
            BuiltInMachines::Produce,
            ProgramInfo {
                hardcoded_inputs: btree_map! {},
                ..ProgramInfo::empty()
            },
        ),
    ));

    let tracer2 = tiles.insert(TileProgram::Machine(
        Dir::West,
        MachineInfo::BuiltIn(BuiltInMachines::Trace, ProgramInfo::empty()),
    ));

    let clock_uuid = uuid::Uuid::new_v4();
    let audio_uuid = uuid::Uuid::new_v4();

    let test_prog = TilemapProgram {
        spec: Tilemap {
            tiles,
            map: arr2(&[
                [
                    None,
                    None,
                    None,
                    None,
                    Some(id_laser_test),
                    None,
                    None,
                    None,
                    None,
                ],
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
        },
        inputs: vec![(clock_uuid, "clock".to_owned(), DataType::Number)],
        outputs: vec![(audio_uuid, "audio".to_owned(), DataType::Number)],
    };

    let test_world = evaluate(test_prog.clone(), hash_map! {clock_uuid: Data::Number(0)}).0;

    commands.insert_resource(test_prog);
    commands.insert_resource(test_world);
     */
    let test_world = test_prog.clone().into_world(vec![], vec![]);

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
        .world
        .map
        .indexed_iter()
        .map(|(index, _)| (TileType::Real, (index.1, index.0)))
        .chain((0..tilemap.world.map.dim().1).map(|x| (TileType::Borderland, (x, 0))))
        .chain(
            (0..tilemap.world.map.dim().1)
                .map(|x| (TileType::Borderland, (x, tilemap.world.map.dim().0 - 1))),
        )
        .chain((1..tilemap.world.map.dim().0 - 1).map(|y| (TileType::Borderland, (0, y))))
        .chain(
            (1..tilemap.world.map.dim().0 - 1)
                .map(|y| (TileType::Borderland, (tilemap.world.map.dim().1 - 1, y))),
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
                    sprite: Sprite::new(bevy::prelude::Vec2::new(10., 10.)),
                    ..Default::default()
                })
                .with(pos)
                .with(size)
                .with_children(|parent| {
                    parent.spawn(Text2dBundle {
                        text: Text::with_section(
                            format!("{}, {}", location.0, location.1),
                            TextStyle {
                                font: asset_server.load("fonts/FiraCode/FiraCode-Light.ttf"),
                                font_size: 45.0,
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

fn size_scaling(
    windows: Res<Windows>,
    mut q: Query<(&TileSize, &mut Sprite)>,
    tilemap: Res<TilemapWorld>,
) {
    let world_extent = tilemap.world_dim();
    let world_extent = world_extent + Extent2::new(2, 2);
    let window = windows.get_primary().unwrap();

    for (sprite_size, mut sprite) in q.iter_mut() {
        sprite.size = bevy::prelude::Vec2::new(
            sprite_size.width as f32 / world_extent.w as f32 * window.width() as f32,
            sprite_size.height as f32 / world_extent.h as f32 * window.height() as f32,
        );
    }
}

fn positioning(
    windows: Res<Windows>,
    mut q: Query<(&TilePosition, &mut Transform)>,
    tilemap: Res<TilemapWorld>,
) {
    let arena_extent = tilemap.world_dim();
    let arena_extent = arena_extent + Extent2::new(2, 2);
    let window = windows.get_primary().unwrap();

    fn convert(pos: usize, bound_window: f32, bound_game: f32) -> f32 {
        let tile_size = bound_window / bound_game;
        ((pos + 1) as f32) / bound_game * bound_window - (bound_window / 2.) + (tile_size / 2.)
    }

    for (pos, mut transform) in q.iter_mut() {
        transform.translation = bevy::prelude::Vec3::new(
            convert(pos.x, window.width() as f32, arena_extent.w as f32),
            convert(pos.y, window.height() as f32, arena_extent.h as f32),
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
        let tile = tilemap
            .world
            .get(Vec2::new(tile_position.x, tile_position.y))
            .map(|(_, _, t)| t);
        *color_mat_handle = get_tile_material(&tile, &materials);
    }
}

fn tile_text(
    mut q: Query<(&TilePosition, &Children)>,
    mut text_q: Query<&mut Text>,
    tilemap: Res<TilemapWorld>,
) {
    for (tile_position, children) in q.iter_mut() {
        let tile = tilemap
            .world
            .get(Vec2::new(tile_position.x, tile_position.y))
            .map(|(_, _, t)| t);
        for child in children.iter() {
            if let Ok(mut text) = text_q.get_mut(*child) {
                text.sections[0].value = match tile {
                    /*Some(TileWorld::Prog(TileProgramMachineInfo::LaserProducer(dir, data))) => {
                        format!("{} {}", dir.to_arrow(), data.show())
                    }*/
                    Some(TileWorld::Prog(TileProgramMachineInfo::Machine(
                        MachineInfo::BuiltIn(_, data),
                    ))) => {
                        if let Some(text) = data.display.clone() {
                            format!("{}", text)
                        } else {
                            "".to_string()
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

    mut tilemap_world: ResMut<TilemapWorld>,
    tilemap_program: Res<TilemapProgram>,
    conn_out: Res<Mutex<midir::MidiOutputConnection>>,
    mut notes_to_end_queue: ResMut<NotesToEnd>,
) {
    /*
    if timer.0.tick(time.delta_seconds()).finished() {
        let mut conn_out = conn_out.lock().unwrap();
        *clock = CurrentClock(clock.0 + 1);

        let new_prog = tilemap_program.clone();
        let clock_uuid = new_prog.inputs.get(0).unwrap().0;
        let (new_world, output) = evaluate(new_prog, hash_map! {clock_uuid: Data::Number(clock.0)});

        start_note(
            &mut conn_out,
            &mut notes_to_end_queue,
            MusicTime::new::<beat>(1.0),
            66,
            0x64,
        );
        *tilemap_world = simulate_until_stable(new_world);
    }
     */
}

fn end_started_notes(
    conn_out: Res<Mutex<midir::MidiOutputConnection>>,
    time: Res<Time>,
    mut notes_to_end: ResMut<NotesToEnd>,
) {
    let mut conn_out = conn_out.lock().unwrap();

    *notes_to_end = NotesToEnd(
        notes_to_end
            .clone()
            .0
            .into_iter()
            .filter_map(|(mut timer, pitch, velocity)| {
                if timer.tick(time.delta_seconds()).finished() {
                    end_note(&mut conn_out, pitch, velocity);
                    None
                } else {
                    Some((timer, pitch, velocity))
                }
            })
            .collect(),
    );
}

fn start_note(
    conn_out: &mut MutexGuard<midir::MidiOutputConnection>,
    notes_to_end_queue: &mut NotesToEnd,
    duration: MusicTime,
    pitch: u8,
    velocity: u8,
) {
    // We're ignoring errors in here
    let duration = (1000.0 as f64 * duration.get::<beat>() / (BEATS_PER_SECOND as f64)) as u64;
    println!("Playing note for {}ms", duration);
    let _ = conn_out.send(&[NOTE_ON_MSG, pitch, velocity]);
    notes_to_end_queue.0.push((
        Timer::new(Duration::from_millis(duration), false),
        pitch,
        velocity,
    ))
}

fn end_note(conn_out: &mut MutexGuard<midir::MidiOutputConnection>, pitch: u8, velocity: u8) {
    // We're ignoring errors in here
    let _ = conn_out.send(&[NOTE_OFF_MSG, pitch, velocity]);
}
