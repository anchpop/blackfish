use std::collections::HashMap;
use std::fmt::Debug;

use bevy::prelude::*;
use ndarray::Array2;

pub struct TilePosition {
    pub x: u32,
    pub y: u32,
}

pub struct TileSize {
    pub width: u32,
    pub height: u32,
}

pub struct Materials {
    pub tiles: HashMap<TileWorld, Handle<ColorMaterial>>,
    pub empty: Handle<ColorMaterial>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TileProgram {
    LaserProducer,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TilePhysics {
    Laser,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TileWorld {
    Phys(TilePhysics),
    Prog(TileProgram),
}

pub type Tile = Option<TileWorld>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TilemapProgram {
    pub map: Array2<Option<TileProgram>>,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TilemapWorld {
    pub map: Array2<Option<TileWorld>>,
}
