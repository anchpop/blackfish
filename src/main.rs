mod types;
mod world_sim;

use std::borrow::BorrowMut;

use types::*;

use bevy::prelude::*;
use ndarray::arr2;

fn main() {
    App::build()
        .add_startup_system(setup.system())
        .add_startup_system(create_map.system())
        .add_startup_stage("game_setup", SystemStage::single(spawn_main_tile.system())) // Adding a stage lets us access resources (in this case, materials) created in the previous stage
        .add_plugins(DefaultPlugins)
        .add_system(size_scaling.system())
        .add_system(positioning.system())
        .add_system(tile_appearance.system())
        .run();
}

fn setup(commands: &mut Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.spawn(Camera2dBundle::default());
    commands.insert_resource(Materials {
        empty: materials.add(Color::rgb(0.1, 0.1, 0.1).into()),
        tiles: [(
            TileWorld::Prog(TileProgram::LaserProducer),
            materials.add(Color::rgb(0.3, 0.3, 0.3).into()),
        )]
        .iter()
        .cloned()
        .collect(),
    });
}

fn create_map(commands: &mut Commands) {
    let test_prog = arr2(&[
        [Some(TileProgram::LaserProducer), None, None],
        [None, None, None],
    ]); // how to make 2d array

    let test_world = test_prog.map(|a| a.clone().map(TileWorld::Prog));

    commands.insert_resource(TilemapProgram { map: test_prog });
    commands.insert_resource(TilemapWorld { map: test_world });
}

fn spawn_main_tile(commands: &mut Commands, materials: Res<Materials>, tilemap: Res<TilemapWorld>) {
    for (index, tile) in tilemap.map.indexed_iter() {
        let pos = TilePosition {
            x: index.1 as u32,
            y: index.0 as u32,
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
            .with(block);
    }
}

fn size_scaling(
    windows: Res<Windows>,
    mut q: Query<(&TileSize, &mut Sprite)>,
    tilemap: Res<TilemapWorld>,
) {
    let (arena_tiles_tall, arena_tiles_wide) = tilemap.map.dim();
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
    let (arena_tiles_tall, arena_tiles_wide) = tilemap.map.dim();
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
        Some(t) => materials.tiles[t].clone(),
    }
}

fn tile_type(mut q: Query<(&TilePosition, &mut Tile)>, world: Res<TilemapWorld>) {
    for (pos, mut tile) in q.iter_mut() {
        *tile = world.map[(pos.y as usize, pos.x as usize)].clone();
    }
}

fn tile_appearance(mut q: Query<(&Tile, &mut Handle<ColorMaterial>)>, materials: Res<Materials>) {
    for (tile, mut color_mat_handle) in q.iter_mut() {
        *color_mat_handle = get_tile_material(tile, &materials);
    }
}
