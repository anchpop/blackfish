mod evaluation;
mod geom;
mod test;
mod types;
mod units;
#[macro_use]
extern crate uom;

use crate::geom::direction::*;
use bevy::{input::keyboard::KeyboardInput, prelude::*, render::camera::Camera};
use frunk::monoid::Monoid;
use geom::{Extent2, Vec2, Vec2i};
use midir::{MidiOutput, MidiOutputPort};
use std::{
    collections::{HashMap, HashSet},
    error::Error,
    io::{stdin, stdout, Write},
    sync::{Mutex, MutexGuard},
    time::Duration,
};
use test::default_program;
use types::{data::*, tilemaps::*, tiles::*};
use units::{
    f64::*,
    music_time::{bang, beat},
};
use velcro::hash_map;

const NOTE_ON_MSG: u8 = 0x90;
const NOTE_OFF_MSG: u8 = 0x80;

const BEATS_PER_SECOND: u64 = 1;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileSize(pub Extent2);
pub struct TileMaterials {
    pub tiles: HashMap<&'static str, Handle<ColorMaterial>>,
    pub empty: Handle<ColorMaterial>,
    pub io_nothing: Handle<ColorMaterial>,
    pub io_connected: Handle<ColorMaterial>,
    pub io_used: Handle<ColorMaterial>,
    pub transparent: Handle<ColorMaterial>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileFromBorder(usize, Dir);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileFromMap(Vec2);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileForPicking(Vec2);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Placing(Option<Vec2>, Dir, usize);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Hotbar(Vec<TileProgram>);
impl std::ops::Index<usize> for Hotbar {
    type Output = TileProgram;

    fn index(&self, i: usize) -> &Self::Output {
        &self.0[i]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MainCamera;

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
        .add_startup_system(create_ui.system())
        // Adding a stage lets us access resources (in this case, materials) created in the previous stage
        .add_startup_stage(
            "game_setup",
            SystemStage::single(spawn_main_tilemap_sprites.system()),
        )
        .add_plugins(DefaultPlugins)
        .add_system(clock_increment.system())
        .add_system(end_started_notes.system())
        .add_system(positioning.system())
        .add_system(tile_appearance.system())
        .add_system(tile_text.system())
        .add_system(picker_follow_mouse.system())
        .add_system(place_block.system())
        .add_system(keyboard_input_system.system())
        .add_system(render_hotbar.system())
        .add_system(rotate_hotbar.system())
        .run();
}

const CLEAR_COLOR: Color = Color::rgb(0.118, 0.122, 0.149);
const EMPTY_COLOR: Color = Color::rgb(49. / 255., 53. / 255., 52. / 255.);
const ID_MACHINE_COLOR: Color = Color::rgb(42. / 255., 183. / 255., 202. / 255.);
const IO_COLOR: Color = Color::rgb(255. / 255., 111. / 255., 89. / 255.);
const IO_EMPTY_COLOR: Color = Color::rgb(55. / 255., 62. / 255., 67. / 255.);
const IO_CONNECTED_COLOR: Color = Color::rgb(80. / 255., 83. / 255., 90. / 255.);
const TRACE_COLOR: Color = Color::rgb(0.5, 0.3, 0.5);
const CONSTANT_COLOR: Color = Color::rgb(254. / 255., 215. / 255., 102. / 255.);
const TRANSPAENT_COLOR: Color = Color::rgba(0., 0., 0., 0.);

fn setup(mut commands: Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.insert_resource(CurrentClock(0));

    commands.insert_resource(ClearColor(CLEAR_COLOR));

    let cam = commands
        .spawn_bundle(OrthographicCameraBundle::new_2d())
        .insert(MainCamera);

    commands.insert_resource(TileMaterials {
        empty: materials.add(EMPTY_COLOR.into()),
        tiles: [
            (
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Produce(()),
                    ProgramInfo {},
                ))
                .name(),
                materials.add(ID_MACHINE_COLOR.into()),
            ),
            (
                TileWorld::Phys(TilePhysics::Laser(DirMap::empty())).name(),
                materials.add(IO_COLOR.into()),
            ),
            (
                TileProgram::Machine(MachineInfo::BuiltIn(
                    BuiltInMachine::Trace(()),
                    ProgramInfo {},
                ))
                .name(),
                materials.add(TRACE_COLOR.into()),
            ),
            ("Constant", materials.add(CONSTANT_COLOR.into())),
        ]
        .iter()
        .cloned()
        .collect(),
        io_nothing: materials.add(IO_EMPTY_COLOR.into()),
        io_connected: materials.add(IO_CONNECTED_COLOR.into()),
        io_used: materials.add(IO_COLOR.into()),
        transparent: materials.add(TRANSPAENT_COLOR.into()),
    });
}

fn get_midi_ports(mut commands: Commands) {
    fn run(mut commands: Commands) -> Result<(), Box<dyn Error>> {
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

fn create_map(mut commands: Commands) {
    let (prog, key) = {
        let mut prog = default_program();
        let key = prog.constants.insert(NfData::Number(4));
        (prog, key)
    };

    let world = evaluation::evaluate(
        &prog,
        hash_map! {"Clock".to_owned(): Data::Whnf(WhnfData::Number(0))},
    );

    commands.insert_resource(prog);
    commands.insert_resource(world);

    commands.insert_resource(Placing(None, Dir::default(), 0));

    commands.insert_resource(Hotbar(vec![
        TileProgram::Machine(MachineInfo::BuiltIn(
            BuiltInMachine::Produce(()),
            ProgramInfo,
        )),
        TileProgram::Literal(key),
    ]));
}

fn spawn_main_tilemap_sprites(
    mut commands: Commands,
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
            rotation: Quat::IDENTITY,
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
            .spawn_bundle(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(10., 10.)),
                ..Default::default()
            })
            .insert(pos)
            .with_children(|parent| {
                parent.spawn_bundle(text_bundle.clone());
            });
        commands
            .spawn_bundle(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(10., 10.)),
                ..Default::default()
            })
            .insert(TileForPicking(location))
            .with_children(|parent| {
                parent.spawn_bundle(text_bundle.clone());
            });
    }

    for location in (0..tilemap.world_dim().h).flat_map(|i| {
        array::IntoIter::new([TileFromBorder(i, Dir::EAST), TileFromBorder(i, Dir::WEST)])
    }) {
        commands
            .spawn_bundle(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(10., 10.)),
                ..Default::default()
            })
            .insert(location)
            .with_children(|parent| {
                parent.spawn_bundle(text_bundle.clone());
            });
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct HotbarParent;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct HotbarItem(usize);

const HOTBAR_NUM_ITEMS: usize = 2;
const HOTBAR_ITEM_WIDTH: f32 = 30.;
const HOTBAR_ITEM_PADDING: f32 = 2.;
const HOTBAR_VERTICAL_PADDING: f32 = 30.;

const HOTBAR_ITEM_TOTAL_WIDTH: f32 = HOTBAR_ITEM_WIDTH + HOTBAR_ITEM_PADDING;
const HOTBAR_TOTAL_WIDTH: f32 = HOTBAR_ITEM_TOTAL_WIDTH * HOTBAR_NUM_ITEMS as f32;
const HOTBAR_TOTAL_HEIGHT: f32 = HOTBAR_ITEM_TOTAL_WIDTH * HOTBAR_NUM_ITEMS as f32;

fn create_ui(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    commands.spawn_bundle(UiCameraBundle::default());
    // root node
    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                position_type: PositionType::Absolute,
                position: Rect {
                    left: Val::Px(0.),
                    bottom: Val::Px(0.),
                    ..Default::default()
                },
                ..Default::default()
            },
            material: materials.add(Color::NONE.into()),
            ..Default::default()
        })
        .insert(HotbarParent)
        .with_children(|parent| {
            for i in 0..HOTBAR_NUM_ITEMS {
                parent
                    .spawn_bundle(NodeBundle {
                        style: Style {
                            size: Size::new(Val::Px(HOTBAR_ITEM_WIDTH), Val::Px(HOTBAR_ITEM_WIDTH)),
                            position_type: PositionType::Absolute,
                            position: Rect {
                                left: Val::Px(
                                    i as f32 * HOTBAR_ITEM_TOTAL_WIDTH
                                        + (((HOTBAR_NUM_ITEMS + 1) as f32 / 2.) % 1.
                                            * HOTBAR_ITEM_TOTAL_WIDTH),
                                ),
                                bottom: Val::Px(0.),
                                ..Default::default()
                            },
                            border: Rect::all(Val::Px(HOTBAR_ITEM_PADDING)),
                            ..Default::default()
                        },
                        material: materials.add(
                            if i == 0 {
                                Color::rgb(0.2, 0.2, 0.4)
                            } else {
                                Color::rgba(0.2, 0.2, 0.4, 0.5)
                            }
                            .into(),
                        ),
                        ..Default::default()
                    })
                    .with_children(|parent| {
                        parent
                            .spawn_bundle(NodeBundle {
                                style: Style {
                                    size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                                    ..Default::default()
                                },
                                material: materials.add(Color::rgb(0.8, 0.8, 1.0).into()),
                                ..Default::default()
                            })
                            .insert(HotbarItem(i));
                    });
            }
        });
}

fn positioning(
    windows: Res<Windows>,
    mut q: QuerySet<(
        Query<(&TileFromMap, &mut Transform, &mut Sprite)>,
        Query<(&TileForPicking, &mut Transform, &mut Sprite)>,
        Query<(&TileFromBorder, &mut Transform, &mut Sprite)>,
    )>,
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

    for (TileFromMap(pos), mut transform, mut sprite) in q.q0_mut().iter_mut() {
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

    for (TileForPicking(pos), mut transform, mut sprite) in q.q1_mut().iter_mut() {
        // Position
        transform.translation = bevy::prelude::Vec3::new(
            convert_squished(pos.x, window.width() as f32, world_extent.w as f32),
            convert_squished(pos.y, window.height() as f32, world_extent.h as f32),
            1.0,
        );

        // Size
        sprite.size = bevy::prelude::Vec2::new(
            1 as f32 / world_extent.w as f32 * window.width() as f32,
            1 as f32 / world_extent.h as f32 * window.height() as f32,
        );
    }

    for (TileFromBorder(index, dir), mut transform, mut sprite) in q.q2_mut().iter_mut() {
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
    location: Vec2,
    materials: &TileMaterials,
    connections: &evaluation::AllConnections,
    program: &TilemapProgram,
) -> Handle<ColorMaterial> {
    match tile {
        None => {
            if connections.iter().any(|(_, _, connection)| {
                graph_edge_to_tile_lines(connection, program)
                    .iter()
                    .any(|tile_line_dir| {
                        tile_line_dir
                            .tile_line
                            .contains(Vec2i::new(location.x as i64, location.y as i64))
                    })
            }) {
                if connections.iter().any(|(_, _, connection)| {
                    graph_edge_to_tile_lines(connection, program)
                        .iter()
                        .any(|tile_line_dir| {
                            tile_line_dir
                                .tile_line
                                .contains(Vec2i::new(location.x as i64, location.y as i64))
                                && !(connection.0.is_nothing() || connection.1.is_nothing())
                        })
                }) {
                    materials.io_connected.clone()
                } else {
                    materials.io_nothing.clone()
                }
            } else {
                materials.empty.clone()
            }
        }
        Some(t) => materials.tiles[t.name()].clone(),
    }
}

fn tile_appearance(
    mut q: QuerySet<(
        Query<(&TileFromMap, &mut Handle<ColorMaterial>)>,
        Query<(&TileForPicking, &mut Handle<ColorMaterial>)>,
        Query<(&TileFromBorder, &mut Handle<ColorMaterial>)>,
    )>,
    placing: Res<Placing>,
    hotbar: Res<Hotbar>,
    tilemap_program: Res<TilemapProgram>,
    materials: Res<TileMaterials>,
    tilemap_world: Res<TilemapWorld>,
) {
    let connection_map = if let Placing(Some(location), orientation, tile) = *placing {
        tilemap_program
            .clone()
            .try_do_to_map(|map| map.add(location, orientation, hotbar[tile]))
            .unwrap_or(tilemap_program.clone())
    } else {
        tilemap_program.clone()
    };
    let connections = evaluation::get_all_connections(&connection_map);

    for (TileFromMap(position), mut color_mat_handle) in q.q0_mut().iter_mut() {
        let tile = tilemap_world
            .world
            .get(Vec2::new(position.x, position.y))
            .map(|(_, _, t)| t);
        *color_mat_handle =
            get_tile_material(&tile, *position, &materials, &connections, &tilemap_program);
    }

    for (TileFromBorder(index, direction), mut color_mat_handle) in q.q2_mut().iter_mut() {
        if direction.basis == Basis::East {
            if direction.sign == Sign::Negative {
                if *index < tilemap_world.inputs.len() {
                    if tilemap_world.connections.iter().any(|connection| {
                        connection.get_start() == tilemap_world.get_input_grid_line_dir(*index)
                    }) {
                        *color_mat_handle = materials.io_used.clone();
                    } else {
                        *color_mat_handle = materials.io_nothing.clone();
                    }
                } else {
                    *color_mat_handle = materials.transparent.clone();
                }
            } else {
                if *index < tilemap_world.outputs.len() {
                    if tilemap_world.connections.iter().any(|connection| {
                        connection.get_end() == tilemap_world.get_output_grid_line_dir(*index)
                    }) {
                        *color_mat_handle = materials.io_used.clone();
                    } else {
                        *color_mat_handle = materials.io_nothing.clone();
                    }
                } else {
                    *color_mat_handle = materials.transparent.clone();
                }
            }
        }
    }

    for (TileForPicking(position), mut color_mat_handle) in q.q1_mut().iter_mut() {
        *color_mat_handle = materials.transparent.clone();
    }

    let Placing(center, orientation, tile) = *placing;
    if let Some(center) = center {
        let positions =
            tilemap_program
                .spec
                .get_tile_positions(&center, &orientation, &hotbar[tile]);
        if let Some(positions) = positions {
            let positions: HashMap<_, _> = positions.into_iter().collect();
            for (TileForPicking(position), mut color_mat_handle) in q.q1_mut().iter_mut() {
                if let Some(_) = positions.get(position) {
                    *color_mat_handle = get_tile_material(
                        &Some(&hotbar[tile].into_world()),
                        *position,
                        &materials,
                        &connections,
                        &tilemap_program,
                    );
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
    if timer.0.tick(time.delta()).finished() {
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
                if timer.tick(time.delta()).finished() {
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

fn picker_follow_mouse(
    q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
    mut q_hotbar_parent: Query<(&mut Style), With<HotbarParent>>,

    windows: Res<Windows>,
    mut evr_cursor: EventReader<CursorMoved>,
    mut placing: ResMut<Placing>,
    tilemap_world: Res<TilemapWorld>,
) {
    fn block_follow_mouse(
        q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
        windows: Res<Windows>,
        cursor_position: bevy::prelude::Vec2,
        mut placing: ResMut<Placing>,
        tilemap_world: Res<TilemapWorld>,
    ) {
        let map_extents = tilemap_world.world_dim();
        let window = windows.get_primary().unwrap();

        fn invert(pos: f32, bound_window: f32, bound_game: f32) -> usize {
            let tile_size = bound_window / bound_game;

            (((pos - (tile_size / 2.) + (bound_window / 2.)) / bound_window) * bound_game).round()
                as usize
        }

        fn invert_squished(pos: f32, bound_window: f32, bound_game: f32) -> Option<usize> {
            invert(pos, bound_window, bound_game + 2.).checked_sub(1)
        }

        if let Ok((camera, camera_transform)) = q_camera.single() {
            let point =
                Camera::screen_to_point_2d(cursor_position, &windows, camera, camera_transform)
                    .unwrap();

            if let (Some(x), Some(y)) = (
                invert_squished(point.x, window.width(), (map_extents.w) as f32),
                invert_squished(point.y, window.height(), (map_extents.h) as f32),
            ) {
                let location = Vec2::new(x, y);
                if tilemap_world.world.check_in_bounds(location) {
                    placing.0 = Some(location);
                } else {
                    placing.0 = None;
                }
            } else {
                placing.0 = None;
            }
        }
    }

    fn hotbar_follow_mouse(mut hotbar_parent_style: Mut<Style>, cursor_pos: bevy::prelude::Vec2) {
        hotbar_parent_style.position.left = Val::Px(cursor_pos.x - HOTBAR_TOTAL_WIDTH / 2.);
        hotbar_parent_style.position.bottom = Val::Px(cursor_pos.y - HOTBAR_TOTAL_HEIGHT);
    }

    if let Some(cursor) = evr_cursor.iter().next() {
        block_follow_mouse(q_camera, windows, cursor.position, placing, tilemap_world);
        hotbar_follow_mouse(q_hotbar_parent.single_mut().unwrap(), cursor.position);
    }
}

fn render_hotbar(
    mut q: Query<(&mut Handle<ColorMaterial>, &HotbarItem)>,
    placing: Res<Placing>,
    hotbar: Res<Hotbar>,
    materials: Res<TileMaterials>,
) {
    for (mut material, HotbarItem(i)) in q.iter_mut() {
        *material = materials.tiles[hotbar[(*i + placing.2) % HOTBAR_NUM_ITEMS].name()].clone()
    }
}

fn rotate_hotbar(mut placing: ResMut<Placing>, keyboard_input: Res<Input<KeyCode>>) {
    if keyboard_input.just_pressed(KeyCode::Key1) {
        (*placing).2 = (1 + (*placing).2) % HOTBAR_NUM_ITEMS;
    } else if keyboard_input.just_pressed(KeyCode::Key2) {
        (*placing).2 = (2 + (*placing).2) % HOTBAR_NUM_ITEMS;
    }
}

fn place_block(
    mouse_button_input: Res<Input<MouseButton>>,
    placing: Res<Placing>,
    hotbar: Res<Hotbar>,
    mut tilemap_program: ResMut<TilemapProgram>,
) {
    if mouse_button_input.just_pressed(MouseButton::Left) {
        if let Placing(Some(location), orientation, tile) = *placing {
            if let Ok(new_program) = tilemap_program
                .clone()
                .try_do_to_map(|map| map.add(location, orientation, hotbar[tile]))
            {
                *tilemap_program = new_program;
            }
        }
    }

    if mouse_button_input.just_pressed(MouseButton::Right) {
        if let Placing(Some(location), _, _) = *placing {
            let new_program = tilemap_program
                .clone()
                .apply_to_map(|map| map.remove(location));
            *tilemap_program = new_program;
        }
    }
}

fn keyboard_input_system(keyboard_input: Res<Input<KeyCode>>, mut placing: ResMut<Placing>) {
    if keyboard_input.just_pressed(KeyCode::R) {
        let Placing(location, orientation, tile) = *placing;
        *placing = Placing(location, orientation.rotate(&Dir::EAST), tile);
    }
}
