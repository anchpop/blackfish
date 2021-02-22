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
    pub tiles: HashMap<&'static str, Handle<ColorMaterial>>,
    pub empty: Handle<ColorMaterial>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TileProgram {
    LaserProducer(Dir),
}
impl TileProgram {
    pub fn name(&self) -> &'static str {
        match self {
            Self::LaserProducer(_) => "LaserProducer",
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TilePhysics {
    Laser,
}
impl TilePhysics {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Laser => "Laser",
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TileWorld {
    Phys(TilePhysics),
    Prog(TileProgram),
}
impl TileWorld {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Phys(p) => p.name(),
            Self::Prog(p) => p.name(),
        }
    }
}

pub type Tile = Option<TileWorld>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TilemapProgram {
    pub program: Array2<Option<TileProgram>>,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TilemapWorld {
    pub world: Array2<Option<TileWorld>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Data {
    Number(i32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Dir {
    North,
    East,
    South,
    West,
}
impl Dir {
    pub fn to_vector(&self) -> Vec2 {
        match self {
            Self::North => Vec2::new(0., 1.),
            Self::East => Vec2::new(1., 0.),
            Self::South => Vec2::new(0., -1.),
            Self::West => Vec2::new(-1., 0.),
        }
    }
}
