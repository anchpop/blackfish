use bevy::prelude::*;
use ndarray::{arr2, Array2};

const ARENA_WIDTH: u32 = 30;
const ARENA_HEIGHT: u32 = 30;

struct TilePosition {
    x: u32,
    y: u32,
}

struct TileSize {
    width: u32,
    height: u32,
}

struct Materials {
    t1: Handle<ColorMaterial>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TileProgram {
    Laser,
}
#[derive(Debug, Clone, PartialEq, Eq)]
enum TilePhysics {}
#[derive(Debug, Clone, PartialEq, Eq)]
enum TileWorld {
    Phys(TilePhysics),
    Prog(TileProgram),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TilemapProgram {
    map: Array2<Option<TileProgram>>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct TilemapWorld {
    map: Array2<Option<TileWorld>>,
}

fn main() {
    App::build()
        .add_startup_system(setup.system())
        .add_startup_system(create_map.system())
        .add_startup_stage("game_setup", SystemStage::single(spawn_main_tile.system())) // Adding a stage lets us access resources (in this case, materials) created in the previous stage
        .add_plugins(DefaultPlugins)
        .add_system(size_scaling.system())
        .run();
}

fn setup(commands: &mut Commands, mut materials: ResMut<Assets<ColorMaterial>>) {
    commands.spawn(Camera2dBundle::default());
    commands.insert_resource(Materials {
        t1: materials.add(Color::rgb(0.9, 0.9, 0.9).into()),
    });
}

fn create_map(commands: &mut Commands) {
    let test = arr2(&[[Some(TileProgram::Laser), None, None], [None, None, None]]); // how to make 2d array

    commands.insert_resource(TilemapProgram { map: test });
}

fn spawn_main_tile(commands: &mut Commands, materials: Res<Materials>, tilemap: Res<TilemapWorld>) {
    dbg!(tilemap);
    const POS: TilePosition = TilePosition { x: 0, y: 0 };
    const SIZE: TileSize = TileSize {
        width: 1,
        height: 1,
    };

    commands
        .spawn(SpriteBundle {
            material: materials.t1.clone(),
            sprite: Sprite::new(Vec2::new(10., 10.)),
            ..Default::default()
        })
        .with(POS)
        .with(SIZE);
}

fn size_scaling(windows: Res<Windows>, mut q: Query<(&TileSize, &mut Sprite)>) {
    let window = windows.get_primary().unwrap();
    for (sprite_size, mut sprite) in q.iter_mut() {
        sprite.size = Vec2::new(
            sprite_size.width as f32 / ARENA_WIDTH as f32 * window.width() as f32,
            sprite_size.height as f32 / ARENA_HEIGHT as f32 * window.height() as f32,
        );
    }
}

// todo: write positioning function that sets the position according to the TilePosition component
