use bevy::prelude::*;

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

fn main() {
    App::build()
        .add_startup_system(setup.system())
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

fn spawn_main_tile(commands: &mut Commands, materials: Res<Materials>) {
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
