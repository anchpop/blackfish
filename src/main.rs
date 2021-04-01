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
use geom::Vec2i;
use types::data::*;
use types::tilemaps::*;
use types::tiles::*;
use types::*;

use bevy::prelude::*;

use frunk::monoid::Monoid;

use std::time::Duration;

use velcro::hash_map;

use std::error::Error;
use std::io::{stdin, stdout, Write};

use midir::{MidiOutput, MidiOutputPort};

use std::sync::{Mutex, MutexGuard};

use test::{const_prog, default_program};

use units::f64::*;
use units::music_time::{bang, beat};

use std::collections::HashMap;

const NOTE_ON_MSG: u8 = 0x90;
const NOTE_OFF_MSG: u8 = 0x80;

const BEATS_PER_SECOND: u64 = 1;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileSize(pub Extent2);

pub struct TileMaterials {
    pub tiles: HashMap<&'static str, Handle<ColorMaterial>>,
    pub empty: Handle<ColorMaterial>,
    pub io_empty: Handle<ColorMaterial>,
    pub io_used: Handle<ColorMaterial>,
    pub transparent: Handle<ColorMaterial>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileFromBorder(usize, Dir);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileFromMap(Vec2);

#[derive(Debug, Clone)]
struct ClockIncrementTimer(Timer);
impl Default for ClockIncrementTimer {
    fn default() -> Self {
        Self(Timer::new(
            Duration::from_millis(
                (1000.0 as f64 * (MusicTime::new::<bang>(1.0)).get::<beat>()
                    / (BEATS_PER_SECOND as f64)) as u64,
            ),
            true,
        ))
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
        .add_startup_stage(
            "game_setup",
            SystemStage::single(spawn_main_tilemap.system()),
        )
        .add_plugins(DefaultPlugins)
        .add_system(clock_increment.system())
        .add_system(end_started_notes.system())
        .add_system(positioning.system())
        .add_system(tile_appearance.system())
        .add_system(tile_text.system())
        .run();
}

fn setup(commands: &mut Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.insert_resource(CurrentClock(0));

    commands.insert_resource(ClearColor(Color::rgb(0.118, 0.122, 0.149)));

    commands.spawn(OrthographicCameraBundle::new_2d());
    commands.insert_resource(TileMaterials {
        empty: materials.add(Color::rgb(0.29019607, 0.3058, 0.3019).into()),
        tiles: [
            (
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {},
                ))
                .name(),
                materials.add(Color::rgb(0.0549, 0.60392, 0.6549).into()),
            ),
            (
                TileWorld::Phys(TilePhysics::Laser(DirMap::empty())).name(),
                materials.add(Color::rgb(0.996, 0.5411, 0.4431).into()),
            ),
            (
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Trace(()),
                    ProgramInfo {},
                ))
                .name(),
                materials.add(Color::rgb(0.5, 0.3, 0.5).into()),
            ),
            (
                "Constant",
                materials.add(Color::rgb(0.96470, 0.80392, 0.3803).into()),
            ),
        ]
        .iter()
        .cloned()
        .collect(),
        io_empty: materials.add(Color::rgb(0.25019607, 0.2558, 0.2519).into()),
        io_used: materials.add(Color::rgb(0.996, 0.5411, 0.4431).into()),
        transparent: materials.add(Color::rgba(0., 0., 0., 0.).into()),
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

fn create_map(commands: &mut Commands) {
    let test_prog = default_program();

    let test_world = evaluation::evaluate(
        &test_prog,
        hash_map! {"Clock".to_owned(): Data::Whnf(WhnfData::Number(0))},
    );

    commands.insert_resource(test_prog);
    commands.insert_resource(test_world);
}

fn spawn_main_tilemap(
    commands: &mut Commands,
    tilemap: Res<TilemapWorld>,
    asset_server: Res<AssetServer>,
) {
    use std::array;

    let text_bundle = Text2dBundle {
        text: Text::with_section(
            "".to_owned(),
            TextStyle {
                font: asset_server.load("fonts/FiraCode/FiraCode-Light.ttf"),
                font_size: 35.0,
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
    };

    for location in tilemap
        .world
        .map
        .indexed_iter()
        .map(|(index, _)| (Vec2::new(index.1, index.0)))
    {
        let pos = TileFromMap(location);
        commands
            .spawn(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(10., 10.)),
                ..Default::default()
            })
            .with(pos)
            .with_children(|parent| {
                parent.spawn(text_bundle.clone());
            });
    }
    for location in (0..tilemap.world_dim().h).flat_map(|i| {
        array::IntoIter::new([TileFromBorder(i, Dir::EAST), TileFromBorder(i, Dir::WEST)])
    }) {
        commands
            .spawn(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(10., 10.)),
                ..Default::default()
            })
            .with(location)
            .with_children(|parent| {
                parent.spawn(text_bundle.clone());
            });
    }
}

fn positioning(
    windows: Res<Windows>,
    mut q_map: Query<(&TileFromMap, &mut Transform, &mut Sprite)>,
    mut q_border: Query<(&TileFromBorder, &mut Transform, &mut Sprite)>,
    tilemap: Res<TilemapWorld>,
) {
    let world_extent = tilemap.world_dim();
    let world_extent = world_extent + Extent2::new(2, 2);
    let window = windows.get_primary().unwrap();

    fn convert(pos: usize, bound_window: f32, bound_game: f32) -> f32 {
        let tile_size = bound_window / bound_game;
        ((pos) as f32) / bound_game * bound_window - (bound_window / 2.) + (tile_size / 2.)
    }

    fn convert_squished(pos: usize, bound_window: f32, bound_game: f32) -> f32 {
        convert(pos + 1, bound_window, bound_game)
    }

    for (TileFromMap(pos), mut transform, mut sprite) in q_map.iter_mut() {
        // Position
        transform.translation = bevy::prelude::Vec3::new(
            convert_squished(pos.x, window.width() as f32, world_extent.w as f32),
            convert_squished(pos.y, window.height() as f32, world_extent.h as f32),
            0.0,
        );

        // Size
        sprite.size = bevy::prelude::Vec2::new(
            1 as f32 / world_extent.w as f32 * window.width() as f32,
            1 as f32 / world_extent.h as f32 * window.height() as f32,
        );
    }

    for (TileFromBorder(index, dir), mut transform, mut sprite) in q_border.iter_mut() {
        // Position
        let pos = if dir.basis == Basis::East {
            Vec2::new(
                if dir.sign == Sign::Positive {
                    world_extent.w - 1
                } else {
                    0
                },
                *index + 1,
            )
        } else {
            Vec2::new(
                *index + 1,
                if dir.sign == Sign::Positive {
                    world_extent.h - 1
                } else {
                    0
                },
            )
        };

        transform.translation = bevy::prelude::Vec3::new(
            convert(pos.x, window.width() as f32, world_extent.w as f32),
            convert(pos.y, window.height() as f32, world_extent.h as f32),
            0.0,
        );

        // Size
        sprite.size = bevy::prelude::Vec2::new(
            1 as f32 / world_extent.w as f32 * window.width() as f32,
            1 as f32 / world_extent.h as f32 * window.height() as f32,
        );
    }
}

fn get_tile_material(
    tile: &Option<&TileWorld>,
    materials: &TileMaterials,
) -> Handle<ColorMaterial> {
    match tile {
        None => materials.empty.clone(),
        Some(t) => materials.tiles[t.name()].clone(),
    }
}

fn tile_appearance(
    mut q_map: Query<(&TileFromMap, &mut Handle<ColorMaterial>)>,
    mut q_border: Query<(&TileFromBorder, &mut Handle<ColorMaterial>)>,
    materials: Res<TileMaterials>,
    tilemap: Res<TilemapWorld>,
) {
    for (tile_position, mut color_mat_handle) in q_map.iter_mut() {
        let tile = tilemap
            .world
            .get(Vec2::new(tile_position.0.x, tile_position.0.y))
            .map(|(_, _, t)| t);
        *color_mat_handle = get_tile_material(&tile, &materials);
    }

    for (TileFromBorder(index, direction), mut color_mat_handle) in q_border.iter_mut() {
        if direction.basis == Basis::East {
            if direction.sign == Sign::Negative {
                if *index < tilemap.inputs.len() {
                    if tilemap.connections.iter().any(|connection| {
                        connection.get_start() == tilemap.get_input_grid_line_dir(*index)
                    }) {
                        *color_mat_handle = materials.io_used.clone();
                    } else {
                        *color_mat_handle = materials.io_empty.clone();
                    }
                } else {
                    *color_mat_handle = materials.transparent.clone();
                }
            } else {
                if *index < tilemap.outputs.len() {
                    if tilemap.connections.iter().any(|connection| {
                        connection.get_end() == tilemap.get_output_grid_line_dir(*index)
                    }) {
                        *color_mat_handle = materials.io_used.clone();
                    } else {
                        *color_mat_handle = materials.io_empty.clone();
                    }
                } else {
                    *color_mat_handle = materials.transparent.clone();
                }
            }
        }
    }
}

fn tile_text(
    mut q_map: Query<(&TileFromMap, &Children)>,
    mut q_border: Query<(&TileFromBorder, &Children)>,
    mut text_q: Query<&mut Text>,
    tilemap: Res<TilemapWorld>,
) {
    for (tile_position, children) in q_map.iter_mut() {
        let tile_info = tilemap
            .world
            .get(Vec2::new(tile_position.0.x, tile_position.0.y));
        for child in children.iter() {
            if let Ok(mut text) = text_q.get_mut(*child) {
                if let Some((tile_center, tile_orientation, tile_type)) = tile_info {
                    text.sections[0].value = match tile_type {
                        /*Some(TileWorld::Prog(TileProgramMachineInfo::LaserProducer(dir, data))) => {
                            format!("{} {}", dir.to_arrow(), data.show())
                        }*/
                        TileWorld::Prog(TileProgramMachineInfo::Machine(MachineInfo::BuiltIn(
                            _,
                            data,
                        ))) => {
                            if let Some(text) = data.display.clone() {
                                format!("{}", text)
                            } else {
                                "".to_string()
                                    + (if &tile_position.0 == tile_center {
                                        tile_orientation.to_arrow()
                                    } else {
                                        ""
                                    })
                            }
                            //format!("{} {}", dir.to_arrow(), data.show())
                        }
                        _ => "".to_string(),
                    };
                } else {
                    text.sections[0].value = "".to_string();
                }
            }
        }
    }
    for (TileFromBorder(index, direction), children) in q_border.iter_mut() {
        for child in children.iter() {
            if let Ok(mut text) = text_q.get_mut(*child) {
                text.sections[0].value = if direction.basis == Basis::East {
                    if direction.sign == Sign::Negative {
                        if let Some((_, Some(Data::Whnf(whnf_data)))) = tilemap.inputs.get(*index) {
                            format!("{} {}", whnf_data.show(), Dir::EAST.to_arrow())
                        } else {
                            "".to_owned()
                        }
                    } else {
                        if let Some((_, Data::Whnf(whnf_data))) = tilemap.outputs.get(*index) {
                            format!("{} {}", Dir::EAST.to_arrow(), whnf_data.show())
                        } else {
                            "".to_owned()
                        }
                    }
                } else {
                    "".to_owned()
                }
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
    if timer.0.tick(time.delta_seconds()).finished() {
        let mut conn_out = conn_out.lock().unwrap();
        *clock = CurrentClock(clock.0 + 1);

        *tilemap_world = evaluation::evaluate(
            &tilemap_program,
            hash_map! {"Clock".to_owned():   Data::Whnf(WhnfData::Number(clock.0))},
        );

        if let Some((_, Data::Whnf(WhnfData::Number(data)))) = tilemap_world
            .outputs
            .iter()
            .find(|(label, _)| label == "Audio")
        {
            start_note(
                &mut conn_out,
                &mut notes_to_end_queue,
                MusicTime::new::<beat>(1.0),
                *data as u8,
                0x64,
            );
        }
    }
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
