use std::{
    collections::{BTreeMap, HashMap},
    iter,
    num::NonZeroUsize,
};
use std::{fmt::Debug, ops::Neg};

use crate::geom::*;

use bevy::prelude::{ColorMaterial, Handle};
use ndarray::{arr2, Array2};

use slotmap::{new_key_type, Key, SlotMap};

use frunk::monoid::Monoid;
use frunk::semigroup::Semigroup;
use uuid::Uuid;

use std::collections::btree_map::Entry;
use velcro::btree_map;

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

pub mod tiles {
    use super::data::*;
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ProgramInfo {
        pub hardcoded_inputs: BTreeMap<String, Data>,
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct WorldMachineInfo {
        pub program_info: ProgramInfo,
        pub display: Option<String>,
    }

    impl Semigroup for ProgramInfo {
        fn combine(&self, other: &Self) -> Self {
            other.clone()
        }
    }
    impl Monoid for ProgramInfo {
        fn empty() -> Self {
            ProgramInfo {
                hardcoded_inputs: BTreeMap::new(),
            }
        }
    }
    impl Semigroup for WorldMachineInfo {
        fn combine(&self, other: &Self) -> Self {
            WorldMachineInfo {
                program_info: self.program_info.combine(&other.program_info),
                display: self.display.combine(&other.display),
            }
        }
    }

    impl Monoid for WorldMachineInfo {
        fn empty() -> Self {
            WorldMachineInfo {
                program_info: ProgramInfo::empty(),
                display: Option::empty(),
            }
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub enum BuiltInMachines {
        Iffy,
        Trace,
        Produce,
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
    pub enum IOType {
        In(String),
        Out(String),
    }

    impl BuiltInMachines {
        pub fn io(&self) -> BTreeMap<Dir, IOType> {
            match self {
                BuiltInMachines::Iffy => {
                    todo!()
                }
                BuiltInMachines::Trace => {
                    btree_map! {
                        Dir::South: IOType::In("observe".to_string())
                    }
                }
                BuiltInMachines::Produce => {
                    btree_map! {
                        Dir::South: IOType::In("product".to_string())
                    }
                }
            }
        }

        pub fn inputs(&self) -> BTreeMap<Dir, String> {
            self.io()
                .into_iter()
                .filter_map(|(dir, iotype)| {
                    if let IOType::In(io) = iotype {
                        Some((dir, io))
                    } else {
                        None
                    }
                })
                .collect()
        }

        pub fn outputs(&self) -> BTreeMap<Dir, String> {
            self.io()
                .into_iter()
                .filter_map(|(dir, iotype)| {
                    if let IOType::Out(io) = iotype {
                        Some((dir, io))
                    } else {
                        None
                    }
                })
                .collect()
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum MachineInfo<I> {
        BuiltIn(BuiltInMachines, I),
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum TileProgramF<I> {
        Machine(Dir, MachineInfo<I>),
    }
    pub type TileProgram = TileProgramF<ProgramInfo>;
    pub type TileProgramMachineInfo = TileProgramF<WorldMachineInfo>;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum TilePhysics {
        Laser(DirMap<Option<Data>>),
    }

    impl<I> TileProgramF<I> {
        pub fn name(&self) -> &'static str {
            match self {
                Self::Machine(_, MachineInfo::BuiltIn(BuiltInMachines::Iffy, _)) => "Iffy",
                Self::Machine(_, MachineInfo::BuiltIn(BuiltInMachines::Trace, _)) => "Trace",
                Self::Machine(_, MachineInfo::BuiltIn(BuiltInMachines::Produce, _)) => "Producer",
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
        pub fn size(&self) -> Vec2 {
            match self {
                Self::Phys(TilePhysics::Laser(_)) => Vec2::new(1, 1),
                Self::Prog(TileProgramMachineInfo::Machine(_, MachineInfo::BuiltIn(b, _))) => {
                    match b {
                        BuiltInMachines::Iffy => Vec2::new(1, 1),
                        BuiltInMachines::Trace => Vec2::new(1, 1),
                        BuiltInMachines::Produce => Vec2::new(1, 1),
                    }
                }
            }
        }
    }
    impl TileProgram {
        pub fn create_machine_info(self) -> TileProgramMachineInfo {
            match self {
                TileProgram::Machine(dir, machine_info) => TileProgramMachineInfo::Machine(
                    dir,
                    match machine_info {
                        MachineInfo::BuiltIn(machine_type, info) => MachineInfo::BuiltIn(
                            machine_type,
                            WorldMachineInfo {
                                program_info: info,
                                ..WorldMachineInfo::empty()
                            },
                        ),
                    },
                ),
            }
        }
    }
}

pub mod tilemaps {
    use super::data::*;
    use super::tiles::*;
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Edit {
        AddTile(Vec2, TileWorld),
        SetTile(Vec2, TileWorld),
        RemoveTile(Vec2),
        AddLaser(Vec2, DirData),
        Edits(Vec<Edit>),
    }

    new_key_type! { pub struct KeyProgram; }
    new_key_type! { pub struct KeyWorld; }
    new_key_type! { pub struct KeyFunction; }

    // brain blast: all tilemaps have inputs and outputs. it doesn't need to be in the actual tile array though - that can be a consequence of rendering. instead, the TilemapProgram struct should contain information describing its inputs and outputs and have an impl function that takes some inputs, converts it into a TilemapWorld, simulates it, then returns what it outputs (along with possibly the TilemapWorld for display)

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
        pub fn get(&self, location: Vec2) -> Option<&I> {
            if let Some(Some(tile_key)) = self.map.get([location.y, location.x]) {
                self.tiles.get(*tile_key)
            } else {
                None
            }
        }

        pub fn get_unchecked(&self, location: Vec2) -> &I {
            self.get(location).unwrap_or_else(|| {
                panic!(
            "Attempted to access a tile at ({}, {}) but it was not present (out of bounds or None)",
            location.x, location.y
        )
            })
        }

        pub fn get_mut(&mut self, location: Vec2) -> Option<&mut I> {
            if let Some(Some(tile_key)) = self.map.get([location.y, location.x]) {
                self.tiles.get_mut(*tile_key)
            } else {
                None
            }
        }
        pub fn get_unchecked_mut(&mut self, location: Vec2) -> &mut I {
            self.get_mut(location).unwrap_or_else(|| {
                panic!(
            "Attempted to access a tile at ({}, {}) but it was not present (out of bounds or None)",
            location.x, location.y
        )
            })
        }

        pub fn add_tile(&mut self, location: Vec2, tile: I) {
            if self.get(location).is_none() {
                let tile_key = self.tiles.insert(tile);
                self.map[[location.y, location.x]] = Some(tile_key);
            } else {
                panic!("Tried to add a tile where one already exists!")
            }
        }
        pub fn set_tile(&mut self, location: Vec2, tile: I) {
            let tile_key = &mut self.map[[location.y, location.x]];
            if let Some(tile_key) = tile_key {
                if let Some(tile_to_update) = self.tiles.get_mut(*tile_key) {
                    *tile_to_update = tile;
                } else {
                    panic!("tile referenced in tilemap was not present in tile slotmap!")
                }
            } else {
                let tile_key = self.tiles.insert(tile);
                self.map[[location.y, location.x]] = Some(tile_key);
            }
        }
        pub fn remove_tile(&mut self, location: Vec2) {
            match self.map[[location.y, location.x]] {
                None => { /* Nothing to do, no tile at location */ }
                Some(tile_key) => {
                    self.tiles.remove(tile_key);
                    self.map[[location.y, location.x]] = None;
                }
            }
        }

        #[allow(dead_code)]
        pub fn make_slotmap() -> SlotMap<K, I> {
            SlotMap::with_key()
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TilemapProgram {
        pub spec: Tilemap<KeyProgram, TileProgram>,
        pub inputs: Vec<(uuid::Uuid, String, DataType)>,
        pub outputs: Vec<(uuid::Uuid, String, DataType)>,
        //pub functions: SlotMap<KeyFunction, TilemapProgram>, // need to make this work with alpha-equivalence
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TilemapWorld {
        pub data: Tilemap<KeyWorld, TileWorld>,
        pub inputs: Vec<Data>,
    }

    impl TilemapWorld {
        #[allow(dead_code)]
        pub fn world(self) -> Array2<Option<KeyWorld>> {
            self.data.map
        }
        pub fn world_dim(&self) -> Extent2 {
            let dim = self.data.map.dim();
            Extent2::new(dim.1, dim.0)
        }

        pub fn set_tile(&mut self, location: Vec2, tile: TileWorld) {
            self.data.set_tile(location, tile);
        }

        pub fn add_tile(&mut self, location: Vec2, tile: TileWorld) {
            self.data.add_tile(location, tile)
        }

        pub fn remove_tile(&mut self, location: Vec2) {
            self.data.remove_tile(location);
        }

        pub fn add_laser(&mut self, location: Vec2, data: DirData) {
            match self.get(location) {
                None => self.add_tile(
                    location,
                    TileWorld::Phys(TilePhysics::Laser(DirMap::empty())),
                ),
                Some(TileWorld::Phys(TilePhysics::Laser(d2))) => {
                    if data != d2.clone() {
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

        pub fn get_outputs(&self, location: Vec2) -> Option<DirData> {
            if let Some(tile) = self.get(location) {
                Some(match tile {
                    TileWorld::Phys(TilePhysics::Laser(dir_data)) => dir_data.clone(),
                    TileWorld::Prog(TileProgramF::Machine(
                        direction,
                        MachineInfo::BuiltIn(machine_type, info),
                    )) => {
                        let input = self.get_inputs(location);
                        if let Some(input) = input {
                            match machine_type {
                                BuiltInMachines::Produce => DirData::empty()
                                    .update(&direction, Some(input["product"].clone())),
                                _ => DirData::empty(),
                            }
                        } else {
                            DirData::empty()
                        }
                    }
                    TileWorld::Prog(TileProgramF::Machine(_, MachineInfo::BuiltIn(_, _))) => {
                        DirData::empty()
                    }
                })
            } else {
                None
            }
        }

        pub fn get_inputs(&self, location: Vec2) -> Option<BTreeMap<String, Data>> {
            if let Some(TileWorld::Prog(TileProgramMachineInfo::Machine(orientation, machine))) =
                self.get(location)
            {
                match machine {
                    MachineInfo::BuiltIn(
                        machine,
                        WorldMachineInfo {
                            program_info: ProgramInfo { hardcoded_inputs },
                            display: _,
                        },
                    ) => {
                        let inputs = machine.inputs();
                        inputs
                            .into_iter()
                            .map(|(dir, label)| {
                                let rotated_dir = dir.rotate(*orientation);
                                let inp = self.get_input_to_coordinate(location, -rotated_dir);
                                (label, inp)
                            })
                            .map(|(label, data)| {
                                let hardcoded = hardcoded_inputs.get(&label).cloned();
                                (label, hardcoded.or(data))
                            })
                            .map(|(label, data)| {
                                if let Some(data) = data {
                                    Some((label, data))
                                } else {
                                    None
                                }
                            })
                            .collect::<Option<Vec<(String, Data)>>>()
                            .map(|a| a.into_iter().collect::<BTreeMap<String, Data>>())
                    }
                }
            } else {
                todo!()
            }
        }

        pub fn get_input_to_coordinate(&self, location: Vec2, direction: Dir) -> Option<Data> {
            match (location.x == 0, direction, self.inputs.get(location.y)) {
                (true, Dir::East, Some(data)) => Some(data.clone()),
                _ => {
                    if let Some(dir_data) = self.get_outputs((-direction).shift(location)) {
                        dir_data.get(&direction).clone()
                    } else {
                        None
                    }
                }
            }
        }

        pub fn update_machine_info(&mut self, location: Vec2, data: WorldMachineInfo) {
            todo!()
        }

        pub fn get(&self, location: Vec2) -> Option<&TileWorld> {
            self.data.get(location)
        }

        #[allow(dead_code)]
        pub fn getu(&self, location: Vec2) -> &TileWorld {
            self.data.get_unchecked(location)
        }

        #[allow(dead_code)]
        pub fn get_mut(&mut self, location: Vec2) -> Option<&mut TileWorld> {
            self.data.get_mut(location)
        }

        #[allow(dead_code)]
        pub fn getu_mut(&mut self, location: Vec2) -> &mut TileWorld {
            self.data.get_unchecked_mut(location)
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

        pub fn apply_transformation<F: Fn(Option<&TileWorld>, Vec2) -> Option<Edit>>(
            &mut self,
            from: &Self,
            transformation: F,
        ) {
            // Before applying any edits, I actually would like to create one big megalist of edits, and then search for incompatibilities (conflicting instructions on whether to update or remove a tile for instance)
            from.data.map.indexed_iter().for_each(|(index, world_key)| {
                if let Some(edit) = transformation(
                    world_key.map(|key| {
                        from.data
                            .tiles
                            .get(key)
                            .expect("referenced key not found in world!")
                    }),
                    Vec2::new(index.1, index.0),
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
        pub fn new(tilemap: Tilemap<KeyProgram, TileProgram>) -> Self {
            Self {
                spec: tilemap,
                inputs: vec![],
                outputs: vec![],
            }
        }
        pub fn set_tile(&mut self, location: Vec2, tile: TileProgram) {
            self.spec.set_tile(location, tile);
        }

        pub fn add_tile(&mut self, location: Vec2, tile: TileProgram) {
            self.spec.add_tile(location, tile)
        }

        pub fn remove_tile(&mut self, location: Vec2) {
            self.spec.remove_tile(location);
        }

        #[allow(dead_code)]
        pub fn program(self) -> Array2<Option<KeyProgram>> {
            self.spec.map
        }
        #[allow(dead_code)]
        pub fn program_dim(&self) -> Extent2 {
            let dim = self.spec.map.dim();
            Extent2::new(dim.1, dim.0)
        }
        pub fn into_world(self, inputs: HashMap<uuid::Uuid, Data>) -> TilemapWorld {
            let map = self.spec.map;
            let mut tiles = self.spec.tiles;

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
            TilemapWorld {
                data: Tilemap {
                    tiles: world_tiles,
                    map: world_map,
                },
                inputs: self
                    .inputs
                    .iter()
                    .map(|(uuid, _, data_type)| {
                        let input = inputs[uuid].clone();
                        assert!(input.check_types(data_type));
                        input
                    })
                    .collect(),
            }
        }

        pub fn make_slotmap() -> SlotMap<KeyProgram, TileProgram> {
            SlotMap::with_key()
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
}

pub mod data {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum DataType {
        Number,
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
        pub fn check_types(&self, t: &DataType) -> bool {
            match (self, t) {
                (Data::Number(_), DataType::Number) => true,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
    pub enum Dir {
        North,
        East,
        South,
        West,
    }
    impl Dir {
        pub fn to_vector(&self) -> vek::vec::Vec2<i64> {
            match self {
                Self::North => vek::vec::Vec2::new(0, 1),
                Self::East => vek::vec::Vec2::new(1, 0),
                Self::South => vek::vec::Vec2::new(0, -1),
                Self::West => vek::vec::Vec2::new(-1, 0),
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

        pub fn shift(&self, by: Vec2) -> Vec2 {
            let (x, y) = (by.x, by.y);
            let v = self.to_vector();

            // the addition here should behave correctly,
            // see https://stackoverflow.com/questions/53453628/how-do-i-add-a-signed-integer-to-an-unsigned-integer-in-rust
            // and https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1448b2d8f02f844f72864e10dbe98049
            Vec2::new(x.wrapping_add(v.x as usize), y.wrapping_add(v.y as usize))
        }

        fn to_num(&self) -> usize {
            match self {
                Self::North => 0,
                Self::East => 1,
                Self::South => 2,
                Self::West => 3,
            }
        }

        fn from_num(i: usize) -> Self {
            match i {
                0 => Self::North,
                1 => Self::East,
                2 => Self::South,
                3 => Self::West,
                _ => panic!("out of bounds"),
            }
        }

        pub fn rotate(&self, by: Self) -> Self {
            Self::from_num((self.to_num() + by.to_num()) % 4)
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
    impl Default for Dir {
        fn default() -> Self {
            Dir::North
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

        pub fn itemize(self) -> [V; 4] {
            [self.north, self.east, self.south, self.west]
        }

        pub fn deitemize(items: [V; 4]) -> Self {
            let [north, east, south, west] = items;
            DirMap {
                north,
                east,
                south,
                west,
            }
        }

        pub fn rotate(self, dir: &Dir) -> Self {
            Self::deitemize({
                let mut items = self.itemize();
                items.rotate_right(dir.to_num());
                items
            })
        }
    }
    pub type DirData = DirMap<Option<Data>>;

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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Void {}

impl Semigroup for Void {
    fn combine(&self, _: &Self) -> Self {
        match self.clone() {}
    }
}
