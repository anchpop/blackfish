use std::{collections::HashMap, iter, num::NonZeroUsize};
use std::{fmt::Debug, ops::Neg};

use bevy::prelude::*;
use ndarray::{Array, Array2};

use slotmap::{new_key_type, Key, SlotMap};

use frunk::monoid::Monoid;
use frunk::semigroup::Semigroup;

pub type XYPair = (usize, usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Edit {
    AddTile(XYPair, TileWorld),
    SetTile(XYPair, TileWorld),
    RemoveTile(XYPair),
    AddLaser(XYPair, DirData),
    Edits(Vec<Edit>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TilePosition {
    pub x: usize,
    pub y: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileSize {
    pub width: usize,
    pub height: usize,
}

pub struct Materials {
    pub tiles: HashMap<&'static str, Handle<ColorMaterial>>,
    pub empty: Handle<ColorMaterial>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NoInfo {}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WorldMachineInfo {
    pub display: Option<String>,
}

// TODO: move the laserproducer into beind a built-in machine, then come up with two functions, one that lets you set a tile's inputs and one that lets you set its outputs. I think I'll have a function that takes a coordinate and tells you what it's outputting. If it's a laser it just looks, and if it's a machine it just calculates the outputs based on the inputs.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltInMachines {
    Iffy,
    Trace,
    Produce,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MachineInfo<I> {
    BuiltIn(BuiltInMachines, I),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TileProgramF<I> {
    LaserProducer(Dir, Data),
    Machine(MachineInfo<I>),
}
pub type TileProgram = TileProgramF<NoInfo>;
pub type TileProgramMachineInfo = TileProgramF<WorldMachineInfo>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TilePhysics {
    Laser(DirMap<Option<Data>>),
}

impl<I> TileProgramF<I> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::LaserProducer(_, _) => "LaserProducer",
            Self::Machine(MachineInfo::BuiltIn(BuiltInMachines::Iffy, _)) => "Iffy",
            Self::Machine(MachineInfo::BuiltIn(BuiltInMachines::Trace, _)) => "Trace",
            Self::Machine(MachineInfo::BuiltIn(BuiltInMachines::Produce, _)) => "ProducerPrime",
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
    Prog(TileProgramMachineInfo),
}
impl TileWorld {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Phys(p) => p.name(),
            Self::Prog(p) => p.name(),
        }
    }

    #[allow(dead_code)]
    pub fn size(&self) -> XYPair {
        match self {
            Self::Phys(TilePhysics::Laser(_)) => (1, 1),
            Self::Prog(TileProgramMachineInfo::LaserProducer(_, _)) => (1, 1),
            Self::Prog(TileProgramMachineInfo::Machine(MachineInfo::BuiltIn(b, _))) => match b {
                BuiltInMachines::Iffy => (1, 1),
                BuiltInMachines::Trace => (1, 1),
                BuiltInMachines::Produce => (1, 1),
            },
        }
    }
}
impl TileProgram {
    pub fn create_machine_info(self) -> TileProgramMachineInfo {
        match self {
            TileProgram::LaserProducer(dir, data) => {
                TileProgramMachineInfo::LaserProducer(dir, data)
            }
            TileProgram::Machine(machine_info) => {
                TileProgramMachineInfo::Machine(match machine_info {
                    MachineInfo::BuiltIn(machine_type, _) => {
                        MachineInfo::BuiltIn(machine_type, WorldMachineInfo::empty())
                    }
                })
            }
        }
    }
}

new_key_type! { pub struct KeyProgram; }
new_key_type! { pub struct KeyWorld; }

// There's no reason a tilemap couldn't be a functor and an applicative - actually, it'd be cool if it were.
// I suspect it would be useful for the same reason that making tiles applicatives was useful for the
// Tile algebra.
#[derive(Debug, Clone)]
pub struct Tilemap<K: Key, I> {
    pub tiles: SlotMap<K, I>,
    pub map: Array2<Option<K>>,
}
impl<K: Key, I: PartialEq> PartialEq for Tilemap<K, I> {
    // should return true if self and other are alpha-equivalent
    fn eq(&self, other: &Self) -> bool {
        // There's actually no reason that the two Tilemaps being compared need to have the same key type.
        // PartialEq requires they be, but we can write this interior function that doesn't have that restriction
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
    pub fn get(&self, location: XYPair) -> Option<&I> {
        if let Some(Some(tile_key)) = self.map.get([location.1, location.0]) {
            self.tiles.get(*tile_key)
        } else {
            None
        }
    }

    pub fn get_unchecked(&self, location: XYPair) -> &I {
        self.get(location).unwrap_or_else(|| {
            panic!(
            "Attempted to access a tile at ({}, {}) but it was not present (out of bounds or None)",
            location.0, location.1
        )
        })
    }

    pub fn get_mut(&mut self, location: XYPair) -> Option<&mut I> {
        if let Some(Some(tile_key)) = self.map.get([location.1, location.0]) {
            self.tiles.get_mut(*tile_key)
        } else {
            None
        }
    }
    pub fn get_unchecked_mut(&mut self, location: XYPair) -> &mut I {
        self.get_mut(location).unwrap_or_else(|| {
            panic!(
            "Attempted to access a tile at ({}, {}) but it was not present (out of bounds or None)",
            location.0, location.1
        )
        })
    }

    pub fn add_tile(&mut self, location: XYPair, tile: I) {
        if self.get(location).is_none() {
            let tile_key = self.tiles.insert(tile);
            self.map[[location.1, location.0]] = Some(tile_key);
        } else {
            panic!("Tried to add a tile where one already exists!")
        }
    }
    pub fn set_tile(&mut self, location: XYPair, tile: I) {
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
    pub fn remove_tile(&mut self, location: XYPair) {
        match self.map[[location.1, location.0]] {
            None => { /* Nothing to do, no tile at location */ }
            Some(tile_key) => {
                self.tiles.remove(tile_key);
                self.map[[location.1, location.0]] = None;
            }
        }
    }

    #[allow(dead_code)]
    pub fn make_slotmap() -> SlotMap<K, I> {
        SlotMap::with_key()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TilemapProgram(pub Tilemap<KeyProgram, TileProgram>);
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TilemapWorld(pub Tilemap<KeyWorld, TileWorld>);

impl TilemapWorld {
    #[allow(dead_code)]
    pub fn world(self) -> Array2<Option<KeyWorld>> {
        self.0.map
    }
    pub fn world_dim(&self) -> XYPair {
        let dim = self.0.map.dim();
        (dim.1, dim.0)
    }

    pub fn set_tile(&mut self, location: XYPair, tile: TileWorld) {
        self.0.set_tile(location, tile);
    }

    pub fn add_tile(&mut self, location: XYPair, tile: TileWorld) {
        self.0.add_tile(location, tile)
    }

    pub fn remove_tile(&mut self, location: XYPair) {
        self.0.remove_tile(location);
    }

    pub fn add_laser(&mut self, location: XYPair, data: DirData) {
        match self.get(location) {
            None => self.add_tile(
                location,
                TileWorld::Phys(TilePhysics::Laser(DirMap::empty())),
            ),
            Some(TileWorld::Phys(TilePhysics::Laser(d2))) => {
                if data != *d2 {
                    let d2 = d2.clone();
                    self.set_tile(
                        location,
                        TileWorld::Phys(TilePhysics::Laser(data.combine(&d2))),
                    )
                }
            }
            _ => {}
        }
    }

    pub fn get_inputs(&self, location: XYPair, direction: Dir) -> Option<Data> {
        if let Some(dir_data) = self.get_outputs((-direction).shift(location)) {
            dir_data.get(&direction).clone()
        } else {
            None
        }
    }

    pub fn get_outputs(&self, location: XYPair) -> Option<DirData> {
        if let Some(tile) = self.get(location) {
            Some(match tile {
                TileWorld::Phys(TilePhysics::Laser(dir_data)) => dir_data.clone(),
                TileWorld::Prog(TileProgramF::LaserProducer(producer_dir, data)) => {
                    DirData::empty().update(producer_dir, Some(data.clone()))
                }
                TileWorld::Prog(TileProgramF::Machine(MachineInfo::BuiltIn(
                    BuiltInMachines::Produce,
                    _info,
                ))) => {
                    DirData::empty().update(&Dir::North, Some(Data::Number(2))) // TODO: Improve
                }
                TileWorld::Prog(TileProgramF::Machine(MachineInfo::BuiltIn(_, _))) => {
                    DirData::empty()
                }
            })
        } else {
            None
        }
    }

    pub fn update_machine_info(&mut self, location: XYPair, data: WorldMachineInfo) {
        todo!()
    }

    pub fn get(&self, location: XYPair) -> Option<&TileWorld> {
        self.0.get(location)
    }

    #[allow(dead_code)]
    pub fn getu(&self, location: XYPair) -> &TileWorld {
        self.0.get_unchecked(location)
    }

    #[allow(dead_code)]
    pub fn get_mut(&mut self, location: XYPair) -> Option<&mut TileWorld> {
        self.0.get_mut(location)
    }

    #[allow(dead_code)]
    pub fn getu_mut(&mut self, location: XYPair) -> &mut TileWorld {
        self.0.get_unchecked_mut(location)
    }

    pub fn apply_edit(&mut self, edit: Edit) {
        match edit {
            Edit::AddTile(location, tile) => self.add_tile(location, tile),
            Edit::SetTile(location, tile) => self.set_tile(location, tile),
            Edit::RemoveTile(location) => self.remove_tile(location),
            Edit::AddLaser(location, data) => self.add_laser(location, data),
            Edit::Edits(edits) => edits.into_iter().for_each(|edit| self.apply_edit(edit)),
        }
    }

    pub fn apply_transformation<F: Fn(Option<&TileWorld>, XYPair) -> Option<Edit>>(
        &mut self,
        from: &Self,
        transformation: F,
    ) {
        // Before applying any edits, I actually would like to create one big megalist of edits, and then search for incompatibilities (conflicting instructions on whether to update or remove a tile for instance)
        from.0.map.indexed_iter().for_each(|(index, world_key)| {
            if let Some(edit) = transformation(
                world_key.map(|key| {
                    from.0
                        .tiles
                        .get(key)
                        .expect("referenced key not found in world!")
                }),
                (index.1, index.0),
            ) {
                self.apply_edit(edit)
            }
        });
    }

    pub fn make_slotmap() -> SlotMap<KeyWorld, TileWorld> {
        SlotMap::with_key()
    }
}

impl TilemapProgram {
    pub fn set_tile(&mut self, location: XYPair, tile: TileProgram) {
        self.0.set_tile(location, tile);
    }

    pub fn add_tile(&mut self, location: XYPair, tile: TileProgram) {
        self.0.add_tile(location, tile)
    }

    pub fn remove_tile(&mut self, location: XYPair) {
        self.0.remove_tile(location);
    }

    #[allow(dead_code)]
    pub fn program(self) -> Array2<Option<KeyProgram>> {
        self.0.map
    }
    #[allow(dead_code)]
    pub fn program_dim(&self) -> XYPair {
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
                    let world_key =
                        world_tiles.insert(TileWorld::Prog(program_tile.create_machine_info()));
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
    pub fn to_vector(&self) -> (i64, i64) {
        match self {
            Self::North => (0, 1),
            Self::East => (1, 0),
            Self::South => (0, -1),
            Self::West => (-1, 0),
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

    pub fn shift(&self, by: XYPair) -> XYPair {
        let (x, y) = by;
        let v = self.to_vector();

        // the addition here should behave correctly,
        // see https://stackoverflow.com/questions/53453628/how-do-i-add-a-signed-integer-to-an-unsigned-integer-in-rust
        // and https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1448b2d8f02f844f72864e10dbe98049
        (x.wrapping_add(v.0 as usize), y.wrapping_add(v.1 as usize))
    }
}
impl Neg for Dir {
    fn neg(self) -> Self::Output {
        match self {
            Self::North => Self::South,
            Dir::East => Self::West,
            Dir::South => Self::North,
            Dir::West => Self::East,
        }
    }

    type Output = Self;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DirMap<V> {
    pub north: V,
    pub east: V,
    pub south: V,
    pub west: V,
}
impl<V> DirMap<V> {
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
type DirData = DirMap<Option<Data>>;

impl<V> IntoIterator for DirMap<V> {
    type Item = (Dir, V);

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        vec![
            (Dir::North, self.north),
            (Dir::East, self.east),
            (Dir::South, self.south),
            (Dir::West, self.west),
        ]
        .into_iter()
    }
}

impl<V: Semigroup> Semigroup for DirMap<V> {
    fn combine(&self, other: &Self) -> Self {
        DirMap {
            north: self.north.combine(&other.north),
            east: self.east.combine(&other.east),
            south: self.south.combine(&other.south),
            west: self.west.combine(&other.west),
        }
    }
}
impl<V: Monoid> Monoid for DirMap<V> {
    fn empty() -> Self {
        DirMap {
            north: V::empty(),
            east: V::empty(),
            south: V::empty(),
            west: V::empty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Void {}

impl Semigroup for Void {
    fn combine(&self, _: &Self) -> Self {
        match self.clone() {}
    }
}

impl Semigroup for NoInfo {
    fn combine(&self, other: &Self) -> Self {
        other.clone()
    }
}
impl Monoid for NoInfo {
    fn empty() -> Self {
        NoInfo {}
    }
}
impl Semigroup for WorldMachineInfo {
    fn combine(&self, other: &Self) -> Self {
        // todo: improve, bc this is not a valid monoid
        other.clone()
    }
}

impl Monoid for WorldMachineInfo {
    fn empty() -> Self {
        WorldMachineInfo {
            display: Option::empty(),
        }
    }
}

impl Semigroup for Edit {
    fn combine(&self, other: &Self) -> Self {
        match (self, other) {
            (Edit::Edits(e1s), Edit::Edits(e2s)) => {
                Edit::Edits(e1s.iter().chain(e2s).cloned().collect())
            }
            (Edit::Edits(e1s), e2) => {
                Edit::Edits(e1s.iter().chain(iter::once(e2)).cloned().collect())
            }
            (e1, Edit::Edits(e2s)) => {
                Edit::Edits(iter::once(e1).chain(e2s.iter()).cloned().collect())
            }
            (e1, e2) => Edit::Edits(vec![e1.clone(), e2.clone()]),
        }
    }
}

impl Monoid for Edit {
    fn empty() -> Self {
        Edit::Edits(vec![])
    }
}
