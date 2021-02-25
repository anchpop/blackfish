use std::fmt::Debug;
use std::{collections::HashMap, num::NonZeroUsize};

use bevy::prelude::*;
use ndarray::Array2;

use frunk::monoid::Monoid;
use frunk::semigroup::Semigroup;

pub struct TilePosition {
    pub x: usize,
    pub y: usize,
}

pub struct TileSize {
    pub width: usize,
    pub height: usize,
}

pub struct Materials {
    pub tiles: HashMap<&'static str, Handle<ColorMaterial>>,
    pub empty: Handle<ColorMaterial>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltInMachines {
    Iffy,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MachineInfo {
    BuiltIn(BuiltInMachines),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TileProgram {
    LaserProducer(Dir, Data),
    Machine(MachineInfo),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TilePhysics {
    Laser(DirData<Option<Data>>),
}
impl TileProgram {
    pub fn name(&self) -> &'static str {
        match self {
            Self::LaserProducer(_, _) => "LaserProducer",
            Self::Machine(_) => "Machine",
        }
    }
}
impl TilePhysics {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Laser(_) => "Laser",
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

    pub fn size(&self) -> (usize, usize) {
        match self {
            Self::Phys(TilePhysics::Laser(_)) => (1, 1),
            Self::Prog(TileProgram::LaserProducer(_, _)) => (1, 1),
            Self::Prog(TileProgram::Machine(MachineInfo::BuiltIn(BuiltInMachines::Iffy))) => (3, 1),
        }
    }
}

pub type Tile = Option<TileWorld>;

// It's confusing but I'm going to be using column-major order here, so the first coordinate is x and the second is y
pub type IntVector2 = (usize, usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tilemap<I> {
    pub map: Array2<Option<I>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TilemapProgram(pub Tilemap<TileProgram>);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TilemapWorld(pub Tilemap<TileWorld>);

impl TilemapWorld {
    pub fn world(self) -> Array2<Option<TileWorld>> {
        self.0.map
    }
    pub fn new(world: Array2<Option<TileWorld>>) -> Self {
        Self(Tilemap { map: world })
    }
    pub fn world_dim(&self) -> (usize, usize) {
        let dim = self.0.map.dim();
        return (dim.1, dim.0);
    }

    pub fn add_laser(&mut self, location: IntVector2, d1: LaserDirData) {
        let tile = &mut self.0.map[[location.1, location.0]];
        match tile {
            None => *tile = Some(TileWorld::Phys(TilePhysics::Laser(DirData::empty()))),
            Some(TileWorld::Phys(TilePhysics::Laser(d2))) => {
                *tile = Some(TileWorld::Phys(TilePhysics::Laser(d1.combine(d2))))
            }
            _ => {}
        }
    }

    pub fn get(&self, location: IntVector2) -> Option<&Tile> {
        self.0.map.get([location.1, location.0])
    }

    pub fn getu(&self, location: IntVector2) -> &Tile {
        self.0.map.get([location.1, location.0]).expect(&format!(
            "Attempted to access a tile at ({}, {}) but tilemap dimensions are {:?}",
            location.0,
            location.1,
            self.0.map.dim()
        ))
    }
}

impl TilemapProgram {
    pub fn program(self) -> Array2<Option<TileProgram>> {
        self.0.map
    }
    pub fn new(program: Array2<Option<TileProgram>>) -> Self {
        Self(Tilemap { map: program })
    }
    pub fn program_dim(&self) -> (usize, usize) {
        let dim = self.0.map.dim();
        return (dim.1, dim.0);
    }
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
impl Data {
    pub fn show(&self) -> String {
        match self {
            Data::Number(num) => format!("{}", num),
        }
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

    pub fn to_arrow(&self) -> &str {
        match self {
            Self::North => "↑",
            Self::East => "→",
            Self::South => "↓",
            Self::West => "←",
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
type LaserDirData = DirData<Option<Data>>;

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
