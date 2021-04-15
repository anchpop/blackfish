mod evaluation;
mod geom;
mod test;
mod types;
mod units;
#[macro_use]
extern crate uom;

use crate::geom::direction::*;
use bevy::{
    prelude::*,
    render::camera::{Camera, OrthographicProjection, ScalingMode},
};
use geom::{Extent2, Vec2, Vec2i};
use midir::{MidiOutput, MidiOutputPort};
use std::{
    collections::HashMap,
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

impl FromWorld for TileMaterials {
    fn from_world(world: &mut World) -> Self {
        let mut materials = world.get_resource_mut::<Assets<ColorMaterial>>().unwrap();

        TileMaterials {
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
                    TileProgram::Machine(MachineInfo::BuiltIn(
                        BuiltInMachine::Copy(()),
                        ProgramInfo {},
                    ))
                    .name(),
                    materials.add(COPY_MACHINE_COLOR.into()),
                ),
                (
                    TileProgram::Machine(MachineInfo::BuiltIn(
                        BuiltInMachine::Modulo((), ()),
                        ProgramInfo {},
                    ))
                    .name(),
                    materials.add(MODULO_MACHINE_COLOR.into()),
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
                ("Mirror", materials.add(MIRROR_COLOR.into())),
            ]
            .iter()
            .cloned()
            .collect(),
            io_nothing: materials.add(IO_EMPTY_COLOR.into()),
            io_connected: materials.add(IO_CONNECTED_COLOR.into()),
            io_used: materials.add(IO_COLOR.into()),
            transparent: materials.add(TRANSPAENT_COLOR.into()),
        }
    }
}

struct ButtonMaterials {
    normal: Handle<ColorMaterial>,
    hovered: Handle<ColorMaterial>,
    pressed: Handle<ColorMaterial>,
    disabled: Handle<ColorMaterial>,
}

impl FromWorld for ButtonMaterials {
    fn from_world(world: &mut World) -> Self {
        let mut materials = world.get_resource_mut::<Assets<ColorMaterial>>().unwrap();
        ButtonMaterials {
            normal: materials.add(Color::rgb(0.15, 0.15, 0.15).into()),
            hovered: materials.add(Color::rgb(0.25, 0.25, 0.25).into()),
            pressed: materials.add(Color::rgb(0.35, 0.75, 0.35).into()),
            disabled: materials.add(Color::rgba(0.15, 0.15, 0.15, 0.5).into()),
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Placing(Option<(Vec2, Option<KeyProgram>)>, Dir, usize);

impl Default for Placing {
    fn default() -> Self {
        Self(None, Dir::default(), 0)
    }
}

pub struct SelectedBlock(Option<KeyProgram>);
impl Default for SelectedBlock {
    fn default() -> Self {
        Self(None)
    }
}
pub struct MenuState {
    constant_hovered: Option<KeyNamedConstant>,
}
impl Default for MenuState {
    fn default() -> Self {
        Self {
            constant_hovered: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileFromBorder(usize, Dir);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileFromMap(Vec2);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileForPicking(Vec2);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PickerOver {
    Empty,
    Occupied,
}
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
                (1000.0 * (MusicTime::new::<bang>(1.0)).get::<beat>() / (BEATS_PER_SECOND as f64))
                    as u64,
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
        .add_plugins(DefaultPlugins)
        .init_resource::<ButtonMaterials>()
        .init_resource::<Placing>()
        .init_resource::<SelectedBlock>()
        .init_resource::<MenuState>()
        .init_resource::<TileMaterials>()
        .add_startup_system(get_midi_ports.system())
        .add_startup_system(setup.system())
        .add_startup_system(create_map.system())
        .add_startup_system(create_hotbar_ui.system())
        .add_startup_system(add_literal_editing_ui.system())
        // Adding a stage lets us access resources (in this case, materials) created in the previous stage
        .add_startup_stage(
            "game_setup",
            SystemStage::single(spawn_main_tilemap_sprites.system()),
        )
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
        .add_system(resize_camera.system())
        .add_system(position_camera.system())
        .add_system(recreate_constant_list.system())
        .add_system(constant_button.system())
        .run();
}

const CLEAR_COLOR: Color = Color::rgb(0.118, 0.122, 0.149);
const EMPTY_COLOR: Color = Color::rgb(49. / 255., 53. / 255., 52. / 255.);
const ID_MACHINE_COLOR: Color = Color::rgb(42. / 255., 183. / 255., 202. / 255.);
const COPY_MACHINE_COLOR: Color = Color::rgb(22. / 255., 210. / 255., 202. / 255.);
const MODULO_MACHINE_COLOR: Color = Color::rgb(50. / 255., 230. / 255., 100. / 255.);
const IO_COLOR: Color = Color::rgb(254. / 255., 111. / 255., 89. / 255.);
const IO_EMPTY_COLOR: Color = Color::rgb(55. / 255., 62. / 255., 67. / 255.);
const IO_CONNECTED_COLOR: Color = Color::rgb(80. / 255., 83. / 255., 90. / 255.);
const TRACE_COLOR: Color = Color::rgb(0.5, 0.3, 0.5);
const CONSTANT_COLOR: Color = Color::rgb(254. / 255., 215. / 255., 102. / 255.);
const MIRROR_COLOR: Color = Color::rgb(220. / 255., 220. / 255., 220. / 255.);
const TRANSPAENT_COLOR: Color = Color::rgba(0., 0., 0., 0.);

fn setup(mut commands: Commands) {
    commands.insert_resource(CurrentClock(0));

    commands.insert_resource(ClearColor(CLEAR_COLOR));

    commands
        .spawn_bundle(OrthographicCameraBundle {
            orthographic_projection: OrthographicProjection {
                scaling_mode: ScalingMode::FixedVertical,
                scale: 10. * MAP_TO_WORLD_SCALE_FACTOR,
                ..OrthographicCameraBundle::new_2d().orthographic_projection
            },
            ..OrthographicCameraBundle::new_2d()
        })
        .insert(MainCamera);
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
        let _ = prog.constants.insert(NfData::Number(8));
        (prog, key)
    };

    let world = evaluation::evaluate(
        &prog,
        hash_map! {"Clock".to_owned(): Data::Whnf(WhnfData::Number(0))},
    );

    commands.insert_resource(prog);
    commands.insert_resource(world);

    commands.insert_resource(Hotbar(vec![
        TileProgram::Machine(MachineInfo::BuiltIn(
            BuiltInMachine::Produce(()),
            ProgramInfo,
        )),
        TileProgram::Machine(MachineInfo::BuiltIn(BuiltInMachine::Copy(()), ProgramInfo)),
        TileProgram::Machine(MachineInfo::BuiltIn(
            BuiltInMachine::Modulo((), ()),
            ProgramInfo,
        )),
        TileProgram::Literal(key),
        TileProgram::Optic(Optic::Mirror),
    ]));
}

fn spawn_main_tilemap_sprites(
    mut commands: Commands,
    tilemap: Res<TilemapWorld>,
    asset_server: Res<AssetServer>,
) {
    use std::array;

    let text_bundle = |z: f32| Text2dBundle {
        text: Text::with_section(
            "".to_owned(),
            TextStyle {
                font: asset_server.load("fonts/FiraCode/FiraCode-Light.ttf"),
                font_size: 35.,
                color: Color::WHITE,
            },
            TextAlignment {
                vertical: VerticalAlign::Center,
                horizontal: HorizontalAlign::Center,
            },
        ),
        transform: Transform {
            translation: Vec3::new(0., 0., z),
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
        commands
            .spawn_bundle(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(
                    MAP_TO_WORLD_SCALE_FACTOR,
                    MAP_TO_WORLD_SCALE_FACTOR,
                )),
                ..Default::default()
            })
            .insert(TileFromMap(location))
            .with_children(|parent| {
                parent.spawn_bundle(text_bundle(2.));
            });
        commands
            .spawn_bundle(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(
                    MAP_TO_WORLD_SCALE_FACTOR,
                    MAP_TO_WORLD_SCALE_FACTOR,
                )),
                ..Default::default()
            })
            .insert(TileForPicking(location))
            .with_children(|parent| {
                parent.spawn_bundle(text_bundle(4.));
            });
    }

    for location in (0..tilemap.world_dim().h).flat_map(|i| {
        array::IntoIter::new([TileFromBorder(i, Dir::EAST), TileFromBorder(i, Dir::WEST)])
    }) {
        commands
            .spawn_bundle(SpriteBundle {
                sprite: Sprite::new(bevy::prelude::Vec2::new(
                    MAP_TO_WORLD_SCALE_FACTOR,
                    MAP_TO_WORLD_SCALE_FACTOR,
                )),
                ..Default::default()
            })
            .insert(location)
            .with_children(|parent| {
                parent.spawn_bundle(text_bundle(2.));
            });
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ConstantAssignmentUiBox;
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ConstantListUiElement(KeyNamedConstant);

fn add_literal_editing_ui(mut commands: Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    // root node
    commands
        .spawn_bundle(NodeBundle {
            style: Style {
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                justify_content: JustifyContent::SpaceBetween,
                ..Default::default()
            },
            material: materials.add(Color::NONE.into()),
            ..Default::default()
        })
        .with_children(|parent| {
            // left vertical fill (border)
            parent
                .spawn_bundle(NodeBundle {
                    style: Style {
                        size: Size::new(Val::Px(200.0), Val::Percent(100.0)),
                        border: Rect::all(Val::Px(2.0)),
                        ..Default::default()
                    },
                    material: materials.add(Color::rgba(0.65, 0.65, 0.65, 0.1).into()),
                    ..Default::default()
                })
                .with_children(|parent| {
                    // left vertical fill (content)
                    parent
                        .spawn_bundle(NodeBundle {
                            style: Style {
                                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                                align_items: AlignItems::Center,
                                flex_direction: FlexDirection::ColumnReverse,
                                ..Default::default()
                            },
                            material: materials.add(Color::rgba(0.15, 0.15, 0.15, 0.5).into()),
                            ..Default::default()
                        })
                        .insert(ConstantAssignmentUiBox);
                });
        });
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct HotbarParent;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct HotbarItem(usize);

const HOTBAR_NUM_ITEMS: usize = 5;
const HOTBAR_ITEM_WIDTH: f32 = 30.;
const HOTBAR_ITEM_PADDING: f32 = 2.;

const HOTBAR_ITEM_TOTAL_WIDTH: f32 = HOTBAR_ITEM_WIDTH + HOTBAR_ITEM_PADDING;
const HOTBAR_TOTAL_WIDTH: f32 = HOTBAR_ITEM_TOTAL_WIDTH * HOTBAR_NUM_ITEMS as f32;
const HOTBAR_TOTAL_HEIGHT: f32 = HOTBAR_ITEM_TOTAL_WIDTH * HOTBAR_NUM_ITEMS as f32;

fn create_hotbar_ui(
    mut commands: Commands,
    _asset_server: Res<AssetServer>,
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

use std::convert::TryFrom;

const MAP_TO_WORLD_SCALE_FACTOR: f32 = 100.;
fn world_to_map_coord(world_location: bevy::prelude::Vec2) -> Option<Vec2> {
    let coord = world_to_map_coord_i(world_location);
    if let (Ok(x), Ok(y)) = (usize::try_from(coord.x), usize::try_from(coord.y)) {
        Some(Vec2::new(x, y))
    } else {
        None
    }
}
fn world_to_map_coord_i(world_location: bevy::prelude::Vec2) -> Vec2i {
    let world_location = world_location / MAP_TO_WORLD_SCALE_FACTOR;
    Vec2i::new(
        world_location.x.round() as i64,
        world_location.y.round() as i64,
    )
}
fn map_to_world_coord(map_location: Vec2) -> bevy::prelude::Vec2 {
    map_to_world_coord_i(Vec2i::new(map_location.x as i64, map_location.y as i64))
}
fn map_to_world_coord_i(map_location: Vec2i) -> bevy::prelude::Vec2 {
    bevy::prelude::Vec2::new((map_location.x) as f32, (map_location.y) as f32)
        * MAP_TO_WORLD_SCALE_FACTOR
}

type AllTileTypesQuery<'a, 'b, C> = QuerySet<(
    Query<'a, (&'b TileFromMap, C)>,
    Query<'a, (&'b TileForPicking, C)>,
    Query<'a, (&'b TileFromBorder, C)>,
)>;
fn positioning(mut q: AllTileTypesQuery<&mut Transform>, tilemap: Res<TilemapWorld>) {
    let world_extent = tilemap.world_dim();
    for (&TileFromMap(pos), mut transform) in q.q0_mut().iter_mut() {
        // Position
        transform.translation = map_to_world_coord(pos).extend(0.);

        // Size
        /*
        sprite.size = bevy::prelude::Vec2::new(
            1 as f32 / world_extent.w as f32 * window.width() as f32,
            1 as f32 / world_extent.h as f32 * window.height() as f32,
        );*/
    }

    for (&TileForPicking(pos), mut transform) in q.q1_mut().iter_mut() {
        // Position
        transform.translation = map_to_world_coord(pos).extend(0.);

        /*         // Size
        sprite.size = bevy::prelude::Vec2::new(
            1 as f32 / world_extent.w as f32 * window.width() as f32,
            1 as f32 / world_extent.h as f32 * window.height() as f32,
        );*/
    }

    for (&TileFromBorder(index, dir), mut transform) in q.q2_mut().iter_mut() {
        // Position
        let pos = if dir.basis == Basis::East {
            Vec2i::new(
                if dir.sign == Sign::Positive {
                    world_extent.w as i64
                } else {
                    -1
                },
                index as i64,
            )
        } else {
            Vec2i::new(
                index as i64,
                if dir.sign == Sign::Positive {
                    (world_extent.h) as i64
                } else {
                    -1
                },
            )
        };

        transform.translation = map_to_world_coord_i(pos).extend(0.);
        /*
        // Size
        sprite.size = bevy::prelude::Vec2::new(
            1 as f32 / world_extent.w as f32 * window.width() as f32,
            1 as f32 / world_extent.h as f32 * window.height() as f32,
        );*/
    }
}

#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
enum ConnectionType {
    HalfConnected,
    FullyConnected,
}
fn get_connection_type(
    grid_line_dir: GridLineDir,
    connection_info: &ConnectionInfo,
) -> Option<ConnectionType> {
    let connections = connection_info.iter().filter_map(|(connection, info)| {
        if info.contains(grid_line_dir) {
            Some(connection)
        } else {
            None
        }
    });
    let connections = connections.map(|(_, _, (from_connection, to_connection))| {
        if !from_connection.is_nothing() && !to_connection.is_nothing() {
            Some(ConnectionType::FullyConnected)
        } else if !from_connection.is_nothing() {
            Some(ConnectionType::HalfConnected)
        } else {
            None
        }
    });
    connections.max().flatten()
}

fn get_tile_material(
    tile: &Option<&TileWorld>,
    location: Vec2,
    materials: &TileMaterials,
    connection_info: &ConnectionInfo,
    laser_connection_info: &ConnectionInfo,
) -> Handle<ColorMaterial> {
    fn get_connection_at_location(
        location: vek::Vec2<usize>,
        connection_info: &HashMap<
            (GraphNode, GraphNode, (FromConnection, ToConnection)),
            ConnectionPath,
        >,
    ) -> Option<ConnectionType> {
        let dirs = [Dir::NORTH, Dir::EAST, Dir::SOUTH, Dir::WEST];
        dirs.iter()
            .filter_map(|&dir| {
                get_connection_type(GridLineDir::new(location, dir), connection_info)
            })
            .max()
    }
    match tile {
        None => {
            let connection = get_connection_at_location(location, connection_info);
            let laser_connection = get_connection_at_location(location, laser_connection_info);
            match (laser_connection, connection) {
                (Some(ConnectionType::FullyConnected), _) => materials.io_used.clone(),
                (_, Some(ConnectionType::FullyConnected)) => materials.io_connected.clone(),
                (_, Some(ConnectionType::HalfConnected)) => materials.io_nothing.clone(),
                (_, _) => materials.empty.clone(),
            }
        }
        Some(t) => materials.tiles[t.name()].clone(),
    }
}

fn tile_appearance(
    mut q: AllTileTypesQuery<&mut Handle<ColorMaterial>>,
    placing: Res<Placing>,
    hotbar: Res<Hotbar>,
    tilemap_program: Res<TilemapProgram>,
    materials: Res<TileMaterials>,
    tilemap_world: Res<TilemapWorld>,
) {
    let connection_map = if let Placing(Some((location, _)), orientation, tile) = *placing {
        tilemap_program
            .clone()
            .try_do_to_map(|map| map.add(location, orientation, hotbar[tile]))
            .unwrap_or_else(|_| tilemap_program.clone())
    } else {
        tilemap_program.clone()
    };
    let connection_info: ConnectionInfo = evaluation::get_all_connections(&connection_map);
    let laser_connection_info: ConnectionInfo = tilemap_world
        .lasers
        .iter()
        .cloned()
        .map(|connection| {
            (
                connection.clone(),
                tilemap_world.connection_info[&connection].clone(),
            )
        })
        .collect::<HashMap<_, _>>();

    for (&TileFromMap(position), mut color_mat_handle) in q.q0_mut().iter_mut() {
        let tile = tilemap_world
            .world
            .get(Vec2::new(position.x, position.y))
            .map(|(_, _, t)| t);
        *color_mat_handle = get_tile_material(
            &tile,
            position,
            &materials,
            &connection_info,
            &laser_connection_info,
        );
    }

    for (&TileFromBorder(index, direction), mut color_mat_handle) in q.q2_mut().iter_mut() {
        if direction.basis == Basis::East {
            *color_mat_handle = if direction.sign == Sign::Negative {
                if index < tilemap_world.inputs.len() {
                    if laser_connection_info.values().any(|connection| {
                        connection.contains(tilemap_world.get_input_grid_line_dir(index))
                    }) {
                        materials.io_used.clone()
                    } else {
                        match get_connection_type(
                            tilemap_world.get_input_grid_line_dir(index),
                            &connection_info,
                        ) {
                            Some(typ) => match typ {
                                ConnectionType::FullyConnected => materials.io_connected.clone(),
                                ConnectionType::HalfConnected => materials.io_nothing.clone(),
                            },
                            None => materials.empty.clone(),
                        }
                    }
                } else {
                    materials.transparent.clone()
                }
            } else if index < tilemap_world.outputs.len() {
                if laser_connection_info.values().any(|connection| {
                    connection.contains(-tilemap_world.get_output_grid_line_dir(index))
                }) {
                    materials.io_used.clone()
                } else {
                    match get_connection_type(
                        -tilemap_world.get_output_grid_line_dir(index),
                        &connection_info,
                    ) {
                        Some(typ) => match typ {
                            ConnectionType::FullyConnected => materials.io_connected.clone(),
                            ConnectionType::HalfConnected => materials.io_nothing.clone(),
                        },
                        None => materials.empty.clone(),
                    }
                }
            } else {
                materials.transparent.clone()
            }
        }
    }

    for (TileForPicking(_), mut color_mat_handle) in q.q1_mut().iter_mut() {
        *color_mat_handle = materials.transparent.clone();
    }

    let Placing(picker_position, orientation, tile) = *placing;
    if let Some((center, None)) = picker_position {
        let positions =
            tilemap_program
                .spec
                .get_tile_positions(&center, &orientation, &hotbar[tile]);
        if let Some(positions) = positions {
            let positions: HashMap<_, _> = positions.into_iter().collect();
            for (&TileForPicking(position), mut color_mat_handle) in q.q1_mut().iter_mut() {
                if positions.get(&position).is_some() {
                    *color_mat_handle = get_tile_material(
                        &Some(&hotbar[tile].into_world()),
                        position,
                        &materials,
                        &connection_info,
                        &laser_connection_info,
                    );
                }
            }
        }
    }
}

#[allow(clippy::clippy::too_many_arguments)]
fn tile_text(
    q_map: Query<(&TileFromMap, &Children)>,
    q_picking: Query<(&TileForPicking, &Children)>,
    q_border: Query<(&TileFromBorder, &Children)>,
    mut text_q: Query<&mut Text>,
    placing: Res<Placing>,
    hotbar: Res<Hotbar>,
    tilemap_program: Res<TilemapProgram>,
    tilemap_world: Res<TilemapWorld>,
) {
    for (&location, children, tile_info, on_main_map) in q_map
        .iter()
        .map(|(TileFromMap(location), children)| {
            (
                location,
                children,
                tilemap_program
                    .spec
                    .get(Vec2::new(location.x, location.y))
                    .cloned(),
                true,
            )
        })
        .chain(
            q_picking
                .iter()
                .map(|(TileForPicking(location), children)| {
                    (
                        location,
                        children,
                        placing
                            .0
                            .map(|(location, _)| (location, placing.1, hotbar[placing.2])),
                        false,
                    )
                }),
        )
    {
        for child in children.iter() {
            if let Ok(mut text) = text_q.get_mut(*child) {
                let picker_on_free_tile = matches!(*placing, Placing(Some((_, None)), _, _));
                if picker_on_free_tile || on_main_map {
                    if let Some((tile_center, tile_orientation, tile_type)) = tile_info {
                        text.sections[0].value = if location == tile_center {
                            match tile_type {
                                TileProgram::Machine(MachineInfo::BuiltIn(_, _)) => {
                                    tile_orientation.to_arrow().to_string()
                                }
                                TileProgram::Literal(litref) => {
                                    tilemap_program.constants.get(litref).unwrap().show()
                                }
                                _ => "".to_string(),
                            }
                        } else {
                            "".to_string()
                        };
                    } else {
                        text.sections[0].value = "".to_string();
                    }
                } else {
                    text.sections[0].value = "".to_string();
                }
            }
        }
    }

    for (&TileFromBorder(index, direction), children) in q_border.iter() {
        for child in children.iter() {
            if let Ok(mut text) = text_q.get_mut(*child) {
                text.sections[0].value = if direction.basis == Basis::East {
                    if direction.sign == Sign::Negative {
                        if let Some((_, Some(Data::Whnf(whnf_data)))) =
                            tilemap_world.inputs.get(index)
                        {
                            format!("{} {}", whnf_data.show(), Dir::EAST.to_arrow())
                        } else {
                            "".to_owned()
                        }
                    } else if let Some((_, Data::Whnf(whnf_data))) =
                        tilemap_world.outputs.get(index)
                    {
                        format!("{} {}", Dir::EAST.to_arrow(), whnf_data.show())
                    } else {
                        "".to_owned()
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

        if let Some(&(_, Data::Whnf(WhnfData::Number(data)))) = tilemap_world
            .outputs
            .iter()
            .find(|(label, _)| label == "Audio")
        {
            start_note(
                &mut conn_out,
                &mut notes_to_end_queue,
                MusicTime::new::<beat>(1.0),
                data as u8,
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
    let duration = (1000. * duration.get::<beat>() / (BEATS_PER_SECOND as f64)) as u64;
    println!("Playing note for {}ms", duration);
    if false {
        let _ = conn_out.send(&[NOTE_ON_MSG, pitch, velocity]);
        notes_to_end_queue.0.push((
            Timer::new(Duration::from_millis(duration), false),
            pitch,
            velocity,
        ))
    }
}

fn end_note(conn_out: &mut MutexGuard<midir::MidiOutputConnection>, pitch: u8, velocity: u8) {
    // We're ignoring errors in here
    let _ = conn_out.send(&[NOTE_OFF_MSG, pitch, velocity]);
}

fn resize_camera(
    mut q_camera: Query<&mut OrthographicProjection, With<MainCamera>>,
    tilemap_world: Res<TilemapWorld>,
) {
    let mut cam = q_camera.single_mut().unwrap();
    cam.scale = (tilemap_world.world.extents().h) as f32 * MAP_TO_WORLD_SCALE_FACTOR / 2.;
}

fn position_camera(
    mut q_camera: Query<&mut Transform, With<MainCamera>>,
    tilemap_world: Res<TilemapWorld>,
) {
    let mut cam = q_camera.single_mut().unwrap();
    cam.translation = (map_to_world_coord(Vec2::from(tilemap_world.world.extents()))
        .extend(cam.translation.y)
        + bevy::prelude::Vec3::new(0., -1., 0.) * MAP_TO_WORLD_SCALE_FACTOR)
        / 2.;
}

fn render_hotbar(
    mut q: Query<(&mut Handle<ColorMaterial>, &HotbarItem)>,
    placing: Res<Placing>,
    hotbar: Res<Hotbar>,
    materials: Res<TileMaterials>,
) {
    for (mut material, &HotbarItem(i)) in q.iter_mut() {
        *material = materials.tiles[hotbar[(i + placing.2) % HOTBAR_NUM_ITEMS].name()].clone()
    }
}

fn rotate_hotbar(mut placing: ResMut<Placing>, keyboard_input: Res<Input<KeyCode>>) {
    if keyboard_input.just_pressed(KeyCode::Key1) {
        (*placing).2 = (1 + (*placing).2) % HOTBAR_NUM_ITEMS;
    } else if keyboard_input.just_pressed(KeyCode::Key2) {
        (*placing).2 = (2 + (*placing).2) % HOTBAR_NUM_ITEMS;
    }
}

fn picker_follow_mouse(
    q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
    mut q_hotbar_parent: Query<&mut Style, With<HotbarParent>>,

    windows: Res<Windows>,
    mut evr_cursor: EventReader<CursorMoved>,
    placing: ResMut<Placing>,
    tilemap_program: Res<TilemapProgram>,
) {
    fn block_follow_mouse(
        q_camera: Query<(&Camera, &GlobalTransform), With<MainCamera>>,
        windows: Res<Windows>,
        cursor_position: bevy::prelude::Vec2,
        mut placing: ResMut<Placing>,
        tilemap_program: Res<TilemapProgram>,
    ) {
        if let Ok((camera, camera_transform)) = q_camera.single() {
            let world_location =
                Camera::screen_to_point_2d(cursor_position, &windows, camera, camera_transform)
                    .unwrap();

            let map_location = world_to_map_coord(world_location.truncate());

            if let Some(map_location) = map_location {
                if tilemap_program.spec.check_in_bounds(map_location) {
                    placing.0 = Some((map_location, tilemap_program.spec.get_loc(map_location)));
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
        block_follow_mouse(q_camera, windows, cursor.position, placing, tilemap_program);
        hotbar_follow_mouse(q_hotbar_parent.single_mut().unwrap(), cursor.position);
    }
}

fn place_block(
    mouse_button_input: Res<Input<MouseButton>>,
    placing: Res<Placing>,
    hotbar: Res<Hotbar>,
    mut tilemap_program: ResMut<TilemapProgram>,
    mut selected_block: ResMut<SelectedBlock>,
    interaction_query: Query<&Interaction>,
) {
    if !interaction_query
        .iter()
        .any(|interaction| match interaction {
            Interaction::Clicked => true,
            Interaction::Hovered => true,
            Interaction::None => false,
        })
    {
        let selected_block = &mut selected_block.0;

        if mouse_button_input.just_pressed(MouseButton::Left) {
            if let Placing(Some((location, None)), orientation, tile) = *placing {
                if let Ok(new_program) = tilemap_program
                    .clone()
                    .try_do_to_map(|map| map.add(location, orientation, hotbar[tile]))
                {
                    *tilemap_program = new_program;
                    *selected_block = tilemap_program.spec.get_loc(location);
                }
            } else if let Placing(Some((_, key_program @ Some(_))), _, _) = *placing {
                *selected_block = key_program;
            } else {
                *selected_block = None;
            }
        }

        if mouse_button_input.just_pressed(MouseButton::Right) {
            if let Placing(Some((location, Some(_))), _, _) = *placing {
                let new_program = tilemap_program
                    .clone()
                    .apply_to_map(|map| map.remove(location));
                *tilemap_program = new_program;
            }
            if let Some(block) = selected_block {
                if tilemap_program.spec.tiles.get(*block).is_none() {
                    *selected_block = None;
                }
            }
        }
    }
}

fn keyboard_input_system(keyboard_input: Res<Input<KeyCode>>, mut placing: ResMut<Placing>) {
    if keyboard_input.just_pressed(KeyCode::R) {
        let Placing(location, orientation, tile) = *placing;
        *placing = Placing(location, orientation.rotate(&Dir::EAST), tile);
    }
}

#[allow(clippy::clippy::too_many_arguments)]
fn recreate_constant_list(
    mut commands: Commands,
    entities: Query<Entity>,
    constant_box_children: Query<Option<&Children>, With<ConstantAssignmentUiBox>>,
    constant_box: Query<Entity, With<ConstantAssignmentUiBox>>,
    asset_server: Res<AssetServer>,
    tilemap_program: Res<TilemapProgram>,
    button_materials: Res<ButtonMaterials>,
    menu_state: Res<MenuState>,
    selected_block: Res<SelectedBlock>,
) {
    if let Some(constant_box_children) = constant_box_children.single().unwrap() {
        for &child in constant_box_children.iter() {
            commands
                .entity(entities.get(child).unwrap())
                .despawn_recursive()
        }
    }
    let constant_box = constant_box.single().unwrap();
    commands.entity(constant_box).with_children(|parent| {
        for (key_named_constant, constant) in tilemap_program.constants.iter() {
            parent
                .spawn_bundle(ButtonBundle {
                    style: Style {
                        size: Size::new(Val::Px(150.0), Val::Px(65.0)),
                        // horizontally center child text
                        justify_content: JustifyContent::Center,
                        // vertically center child text
                        align_items: AlignItems::Center,
                        ..Default::default()
                    },
                    material: if let SelectedBlock(Some(selected_tile_key)) = *selected_block {
                        let &(_, _, tile) = tilemap_program
                            .spec
                            .tiles
                            .get(selected_tile_key)
                            .expect("referring to a tile that no longer exists!");

                        match tile {
                            TileProgramF::Literal(current_constant)
                                if current_constant == key_named_constant =>
                            {
                                button_materials.pressed.clone()
                            }
                            _ => {
                                if Some(key_named_constant) == menu_state.constant_hovered {
                                    button_materials.hovered.clone()
                                } else {
                                    button_materials.normal.clone()
                                }
                            }
                        }
                    } else {
                        button_materials.disabled.clone()
                    },
                    ..Default::default()
                })
                .insert(ConstantListUiElement(key_named_constant))
                .with_children(|parent| {
                    parent.spawn_bundle(TextBundle {
                        text: Text::with_section(
                            constant.show(),
                            TextStyle {
                                font: asset_server.load("fonts/FiraCode/FiraCode-Light.ttf"),
                                font_size: 40.0,
                                color: Color::rgb(0.9, 0.9, 0.9),
                            },
                            Default::default(),
                        ),
                        ..Default::default()
                    });
                });
        }
    });
}

type InteractionQuery<'a, 'b> =
    Query<'a, (&'b Interaction, &'b ConstantListUiElement), (Changed<Interaction>, With<Button>)>;
fn constant_button(
    mut interaction_query: InteractionQuery,
    mut tilemap_program: ResMut<TilemapProgram>,
    mut selected_block: ResMut<SelectedBlock>,
    mut menu_state: ResMut<MenuState>,
) {
    for (interaction, &ConstantListUiElement(key_named_constant)) in interaction_query.iter_mut() {
        let selected_block = &mut selected_block.0;
        if let Some(selected_tile_key) = selected_block {
            let &(location, _, tile) = tilemap_program
                .spec
                .tiles
                .get(*selected_tile_key)
                .expect("referring to a tile that no longer exists!");
            if let TileProgramF::Literal(_) = tile {
                match *interaction {
                    Interaction::Clicked => {
                        if let Ok(new_tilemap_program) =
                            tilemap_program.clone().try_do_to_map(|map| {
                                let new_map = map.update(location, |tile_info| {
                                    let &(location, orientation, _) = tile_info.unwrap();
                                    Some((
                                        location,
                                        orientation,
                                        TileProgram::Literal(key_named_constant),
                                    ))
                                })?;
                                Ok(new_map)
                            })
                        {
                            *tilemap_program = new_tilemap_program;
                            *selected_tile_key = tilemap_program.spec.get_loc(location).unwrap();
                        }
                    }
                    Interaction::Hovered => menu_state.constant_hovered = Some(key_named_constant),
                    Interaction::None => {
                        if menu_state.constant_hovered == Some(key_named_constant) {
                            menu_state.constant_hovered = None
                        }
                    }
                }
            }
        }
    }
}
