#![feature(or_patterns)]

mod types;
mod world_sim;

use types::*;
use world_sim::sim;

use bevy::prelude::*;
use ndarray::arr2;

use frunk::monoid::Monoid;

fn main() {
    App::build()
        .add_startup_system(setup.system())
        .add_startup_system(create_map.system())
        .add_startup_stage("game_setup", SystemStage::single(spawn_main_tile.system())) // Adding a stage lets us access resources (in this case, materials) created in the previous stage
        .add_plugins(DefaultPlugins)
        .add_system(size_scaling.system())
        .add_system(positioning.system())
        .add_system(tile_appearance.system())
        .add_system(tile_text.system())
        .run();
}

fn setup(commands: &mut Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.spawn(OrthographicCameraBundle::new_2d());
    commands.insert_resource(Materials {
        empty: materials.add(Color::rgb(0.1, 0.1, 0.1).into()),
        tiles: [
            (
                TileWorld::Prog(TileProgram::LaserProducer(Dir::North, Data::Number(2))).name(),
                materials.add(Color::rgb(0.3, 0.3, 0.3).into()),
            ),
            (
                TileWorld::Phys(TilePhysics::Laser(DirData::empty())).name(),
                materials.add(Color::rgb(0.9, 0.3, 0.3).into()),
            ),
        ]
        .iter()
        .cloned()
        .collect(),
    });
}

fn create_map(commands: &mut Commands) {
    let test_prog = TilemapProgram::new(arr2(&[
        [None, None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None, None],
        [
            None,
            None,
            None,
            Some(TileProgram::LaserProducer(Dir::North, Data::Number(2))),
            None,
            Some(TileProgram::LaserProducer(Dir::West, Data::Number(2))),
            None,
            None,
            None,
        ],
        [None, None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None, None],
        [
            None,
            None,
            None,
            None,
            None,
            None,
            None,
            Some(TileProgram::LaserProducer(Dir::West, Data::Number(2))),
            None,
        ],
        [None, None, None, None, None, None, None, None, None],
        [None, None, None, None, None, None, None, None, None],
    ]));

    let test_world = sim(test_prog.clone());

    commands.insert_resource(test_prog);
    commands.insert_resource(test_world);
}

fn spawn_main_tile(
    commands: &mut Commands,
    materials: Res<Materials>,
    tilemap: Res<TilemapWorld>,
    asset_server: Res<AssetServer>,
) {
    for (index, tile) in tilemap.0.map.indexed_iter() {
        let pos = TilePosition {
            x: index.1 as usize,
            y: index.0 as usize,
        };
        let size = TileSize {
            width: 1,
            height: 1,
        };
        let block: Tile = tile.clone();
        commands
            .spawn(SpriteBundle {
                material: get_tile_material(&block, &materials),
                sprite: Sprite::new(Vec2::new(10., 10.)),
                ..Default::default()
            })
            .with(pos)
            .with(size)
            .with(block)
            .with_children(|parent| {
                parent.spawn(Text2dBundle {
                    text: Text::with_section(
                        format!("{}, {}", index.1, index.0),
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

fn size_scaling(
    windows: Res<Windows>,
    mut q: Query<(&TileSize, &mut Sprite)>,
    tilemap: Res<TilemapWorld>,
) {
    let (arena_tiles_wide, arena_tiles_tall) = tilemap.world_dim();
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
    let window = windows.get_primary().unwrap();

    fn convert(pos: f32, bound_window: f32, bound_game: f32) -> f32 {
        let tile_size = bound_window / bound_game;
        pos / bound_game * bound_window - (bound_window / 2.) + (tile_size / 2.)
    }

    for (pos, mut transform) in q.iter_mut() {
        transform.translation = Vec3::new(
            convert(pos.x as f32, window.width() as f32, arena_tiles_wide as f32),
            convert(
                pos.y as f32,
                window.height() as f32,
                arena_tiles_tall as f32,
            ),
            0.0,
        );
    }
}

fn get_tile_material(tile: &Tile, materials: &Materials) -> Handle<ColorMaterial> {
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
        let tile = tilemap.getu((tile_position.x, tile_position.y));
        *color_mat_handle = get_tile_material(tile, &materials);
    }
}

fn tile_text(
    mut q: Query<(&TilePosition, &Children)>,
    mut text_q: Query<&mut Text>,
    tilemap: Res<TilemapWorld>,
) {
    for (tile_position, children) in q.iter_mut() {
        let tile = tilemap.getu((tile_position.x, tile_position.y));
        for child in children.iter() {
            if let Ok(mut text) = text_q.get_mut(*child) {
                text.sections[0].value = match tile {
                    Some(TileWorld::Prog(TileProgram::LaserProducer(dir, data))) => {
                        format!("{} {}", dir.to_arrow(), data.show())
                    }
                    _ => "".to_string(),
                };
            }
        }
    }
}
