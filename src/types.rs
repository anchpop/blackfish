use std::fmt::Debug;
use std::{collections::HashMap, num::NonZeroUsize};

use bevy::prelude::*;
use ndarray::{Array, Array2};

use slotmap::{new_key_type, Key, SlotMap};

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

// It's confusing but I'm going to be using column-major order here, so the first coordinate is x and the second is y
pub type IntVector2 = (usize, usize);

new_key_type! { pub struct KeyProgram; }
new_key_type! { pub struct KeyWorld; }
#[derive(Debug, Clone)]
pub struct Tilemap<K: Key, I> {
    pub tiles: SlotMap<K, I>,
    pub map: Array2<Option<K>>,
}
impl<K: Key, I: PartialEq> PartialEq for Tilemap<K, I> {
    // should return true if self and other are alpha-equivalent
    fn eq(&self, other: &Self) -> bool {
        // There's actually no reason that the two Tilemaps being compared need to have the same key type.
        // That's not allowed by PartialEq, but we can write this interior function that doesn't have that restriction
        // and just call it. This prevents us from making bugs where we use the second tilemap key when we should have
        // used the first.
        fn e<Kp: Key, Kpp: Key, Ip: PartialEq>(
            s: &Tilemap<Kp, Ip>,
            other: &Tilemap<Kpp, Ip>,
        ) -> bool {
            if s.map.dim() != other.map.dim() {
                return false;
            }

            let mut associations: HashMap<Kp, Kpp> = HashMap::new();

            for (index, tile_key_self) in s.map.indexed_iter() {
                let tile_key_other = &other.map[index];
                match (tile_key_self, tile_key_other) {
                    (None, None) => {}
                    (Some(tile_key_self), Some(tile_key_other)) => {
                        if let Some(corresponding_tile) = associations.get(tile_key_self) {
                            if corresponding_tile != tile_key_other {
                                return false;
                            }
                        } else {
                            associations.insert(*tile_key_self, *tile_key_other);
                        }

                        let tile_self = s.tiles.get(*tile_key_self);
                        let tile_other = other.tiles.get(*tile_key_other);
                        match (tile_self, tile_other) {
                            (None, None) => {}
                            (Some(tile_self), Some(tile_other)) => {
                                if *tile_self != *tile_other {
                                    return false;
                                }
                            }
                            _ => return false,
                        }
                    }
                    _ => {
                        return false;
                    }
                }
            }

            true
        }
        e(self, other)
    }
}
impl<K: Key, I: Eq> Eq for Tilemap<K, I> {}
impl<K: Key, I> Tilemap<K, I> {
    pub fn get(&self, location: IntVector2) -> Option<&I> {
        if let Some(Some(tile_key)) = self.map.get([location.1, location.0]) {
            self.tiles.get(*tile_key)
        } else {
            None
        }
    }

    pub fn getu(&self, location: IntVector2) -> &I {
        self.get(location).unwrap_or_else(|| {
            panic!(
            "Attempted to access a tile at ({}, {}) but it was not present (out of bounds or None)",
            location.0, location.1
        )
        })
    }

    pub fn get_mut(&mut self, location: IntVector2) -> Option<&mut I> {
        if let Some(Some(tile_key)) = self.map.get([location.1, location.0]) {
            self.tiles.get_mut(*tile_key)
        } else {
            None
        }
    }
    pub fn getu_mut(&mut self, location: IntVector2) -> &mut I {
        self.get_mut(location).unwrap_or_else(|| {
            panic!(
            "Attempted to access a tile at ({}, {}) but it was not present (out of bounds or None)",
            location.0, location.1
        )
        })
    }

    pub fn add_tile(&mut self, location: IntVector2, tile: I) {
        if self.get(location).is_none() {
            let tile_key = self.tiles.insert(tile);
            self.map[[location.1, location.0]] = Some(tile_key);
        } else {
            panic!("Tried to add a tile where one already exists!")
        }
    }
    pub fn set_tile(&mut self, location: IntVector2, tile: I) {
        let tile_key = &mut self.map[[location.1, location.0]];
        if let Some(tile_key) = tile_key {
            if let Some(tile_to_update) = self.tiles.get_mut(*tile_key) {
                *tile_to_update = tile;
            } else {
                panic!("tile referenced in tilemap was not present in tile slotmap!")
            }
        } else {
            let tile_key = self.tiles.insert(tile);
            self.map[[location.1, location.0]] = Some(tile_key);
        }
    }
    pub fn remove_tile(&mut self, location: IntVector2) {
        match self.map[[location.1, location.0]] {
            None => { /* Nothing to do, no tile at location */ }
            Some(tile_key) => {
                self.tiles.remove(tile_key);
                self.map[[location.1, location.0]] = None;
            }
        }
    }
    pub fn make_slotmap() -> SlotMap<K, I> {
        SlotMap::with_key()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TilemapProgram(pub Tilemap<KeyProgram, TileProgram>);
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TilemapWorld(pub Tilemap<KeyWorld, TileWorld>);

impl TilemapWorld {
    pub fn world(self) -> Array2<Option<KeyWorld>> {
        self.0.map
    }
    pub fn world_dim(&self) -> (usize, usize) {
        let dim = self.0.map.dim();
        (dim.1, dim.0)
    }

    pub fn set_tile(&mut self, location: IntVector2, tile: TileWorld) {
        self.0.set_tile(location, tile);
    }
    pub fn remove_tile(&mut self, location: IntVector2) {
        self.0.remove_tile(location);
    }

    pub fn add_tile(&mut self, location: IntVector2, tile: TileWorld) {
        self.0.add_tile(location, tile)
    }

    pub fn add_laser(&mut self, location: IntVector2, d1: LaserDirData) {
        match self.get(location) {
            None => self.add_tile(
                location,
                TileWorld::Phys(TilePhysics::Laser(DirData::empty())),
            ),
            Some(TileWorld::Phys(TilePhysics::Laser(d2))) => {
                if d1 != *d2 {
                    let d2 = d2.clone();
                    self.set_tile(
                        location,
                        TileWorld::Phys(TilePhysics::Laser(d1.combine(&d2))),
                    )
                }
            }
            _ => {}
        }
    }

    pub fn get(&self, location: IntVector2) -> Option<&TileWorld> {
        self.0.get(location)
    }

    pub fn getu(&self, location: IntVector2) -> &TileWorld {
        self.0.getu(location)
    }

    pub fn get_mut(&mut self, location: IntVector2) -> Option<&mut TileWorld> {
        self.0.get_mut(location)
    }
    pub fn getu_mut(&mut self, location: IntVector2) -> &mut TileWorld {
        self.0.getu_mut(location)
    }

    pub fn make_slotmap() -> SlotMap<KeyWorld, TileWorld> {
        SlotMap::with_key()
    }
}

impl TilemapProgram {
    pub fn program(self) -> Array2<Option<KeyProgram>> {
        self.0.map
    }
    pub fn program_dim(&self) -> (usize, usize) {
        let dim = self.0.map.dim();
        (dim.1, dim.0)
    }
    pub fn into_world(self) -> TilemapWorld {
        let map = self.0.map;
        let mut tiles = self.0.tiles;

        let mut world_tiles: SlotMap<KeyWorld, TileWorld> = TilemapWorld::make_slotmap();

        let world_map: Array2<Option<KeyWorld>> = map.mapv(|program_key| match program_key {
            Some(program_key) => {
                if let Some(program_tile) = tiles.remove(program_key) {
                    let world_key = world_tiles.insert(TileWorld::Prog(program_tile));
                    Some(world_key)
                } else {
                    None
                }
            }
            _ => None,
        });
        TilemapWorld(Tilemap {
            tiles: world_tiles,
            map: world_map,
        })
    }

    pub fn make_slotmap() -> SlotMap<KeyProgram, TileProgram> {
        SlotMap::with_key()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Data {
    Number(i32),
}
impl Semigroup for Data {
    fn combine(&self, other: &Self) -> Self {
        other.clone()
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
