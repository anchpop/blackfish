use std::collections::HashMap;
use std::fmt::Debug;

use bevy::prelude::*;
use ndarray::Array2;

use frunk::monoid::Monoid;
use frunk::semigroup::Semigroup;

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
    LaserProducer(Dir, Data),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TilePhysics {
    Laser(DirData<Option<Data>>),
}
impl TileProgram {
    pub fn name(&self) -> &'static str {
        match self {
            Self::LaserProducer(_, _) => "LaserProducer",
        }
    }
}
impl TilePhysics {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Laser(_) => "Laser",
        }
    }
    pub fn empty_laser() -> Self {
        Self::Laser(DirData::empty())
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
impl Semigroup for Data {
    fn combine(&self, other: &Self) -> Self {
        return other.clone();
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirData<V> {
    north: V,
    east: V,
    south: V,
    west: V,
}
impl<V> DirData<V> {
    pub fn get(&self, dir: &Dir) -> &V {
        match dir {
            Dir::North => &self.north,
            Dir::South => &self.south,
            Dir::East => &self.east,
            Dir::West => &self.west,
        }
    }
    pub fn update(self, dir: &Dir, v: V) -> Self {
        match dir {
            Dir::North => Self { north: v, ..self },
            Dir::South => Self { south: v, ..self },
            Dir::East => Self { east: v, ..self },
            Dir::West => Self { west: v, ..self },
        }
    }
}

impl<V: Semigroup> Semigroup for DirData<V> {
    fn combine(&self, other: &Self) -> Self {
        DirData {
            north: self.north.combine(&other.north),
            east: self.east.combine(&other.east),
            south: self.south.combine(&other.south),
            west: self.west.combine(&other.west),
        }
    }
}
impl<V: Monoid> Monoid for DirData<V> {
    fn empty() -> Self {
        DirData {
            north: V::empty(),
            east: V::empty(),
            south: V::empty(),
            west: V::empty(),
        }
    }
}
