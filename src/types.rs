use std::{
    collections::{BTreeMap, HashMap},
    iter,
    num::NonZeroUsize,
};
use std::{fmt::Debug, ops::Neg};

use crate::geom::direction::*;
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
    use crate::geom::tilemap::Shaped;

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
                        Dir::south: IOType::In("observe".to_string())
                    }
                }
                BuiltInMachines::Produce => {
                    btree_map! {
                        Dir::south: IOType::In("product".to_string())
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

    impl<T> tilemap::Shaped for TileProgramF<T> {
        fn shape(&self) -> nonempty::NonEmpty<Vec2i> {
            todo!()
        }
    }

    impl tilemap::Shaped for TilePhysics {
        fn shape(&self) -> nonempty::NonEmpty<Vec2i> {
            todo!()
        }
    }

    impl tilemap::Shaped for TileWorld {
        fn shape(&self) -> nonempty::NonEmpty<Vec2i> {
            match self {
                TileWorld::Phys(t) => t.shape(),
                TileWorld::Prog(t) => t.shape(),
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

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TilemapProgram {
        pub spec: tilemap::Tilemap<KeyProgram, TileProgram>,
        pub inputs: Vec<(uuid::Uuid, String, DataType)>,
        pub outputs: Vec<(uuid::Uuid, String, DataType)>,
        //pub functions: SlotMap<KeyFunction, TilemapProgram>, // need to make this work with alpha-equivalence
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TilemapWorld {
        pub data: tilemap::Tilemap<KeyWorld, TileWorld>,
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
            todo!() //self.data.set_tile(location, tile);
        }

        pub fn add_tile(&mut self, location: Vec2, tile: TileWorld) {
            todo!() //self.data.add_tile(location, tile)
        }

        pub fn remove_tile(&mut self, location: Vec2) {
            todo!() //self.data.remove_tile(location);
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

        /*
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
        */
        /*
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
         */

        pub fn update_machine_info(&mut self, location: Vec2, data: WorldMachineInfo) {
            todo!()
        }

        pub fn get(&self, location: Vec2) -> Option<&TileWorld> {
            todo!() //self.data.get(location)
        }

        #[allow(dead_code)]
        pub fn getu(&self, location: Vec2) -> &TileWorld {
            todo!() //self.data.get_unchecked(location)
        }

        #[allow(dead_code)]
        pub fn get_mut(&mut self, location: Vec2) -> Option<&mut TileWorld> {
            todo!() //self.data.get_mut(location)
        }

        #[allow(dead_code)]
        pub fn getu_mut(&mut self, location: Vec2) -> &mut TileWorld {
            todo!() //self.data.get_unchecked_mut(location)
        }

        pub fn make_slotmap() -> SlotMap<KeyWorld, (Vec2, Dir, TileWorld)> {
            SlotMap::with_key()
        }
    }

    impl TilemapProgram {
        pub fn new(tilemap: tilemap::Tilemap<KeyProgram, TileProgram>) -> Self {
            Self {
                spec: tilemap,
                inputs: vec![],
                outputs: vec![],
            }
        }
        pub fn set_tile(&mut self, location: Vec2, tile: TileProgram) {
            todo!() //self.spec.set_tile(location, tile);
        }

        pub fn add_tile(&mut self, location: Vec2, tile: TileProgram) {
            todo!() //self.spec.add_tile(location, tile)
        }

        pub fn remove_tile(&mut self, location: Vec2) {
            todo!() //self.spec.remove_tile(location);
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

            let mut world_tiles: SlotMap<KeyWorld, (Vec2, Dir, TileWorld)> =
                TilemapWorld::make_slotmap();

            let world_map: Array2<Option<KeyWorld>> = map.mapv(|program_key| match program_key {
                Some(program_key) => {
                    if let Some(program_tile) = tiles.remove(program_key) {
                        let world_key = world_tiles.insert((
                            program_tile.0,
                            program_tile.1,
                            TileWorld::Prog(program_tile.2.create_machine_info()),
                        ));
                        Some(world_key)
                    } else {
                        None
                    }
                }
                _ => None,
            });
            TilemapWorld {
                data: tilemap::Tilemap {
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

        pub fn make_slotmap() -> SlotMap<KeyProgram, (Vec2, Dir, TileProgram)> {
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
        Thunk(GridLineDir),
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
                Data::Thunk(grid_line_dir) => format!("thunk: {:?}", grid_line_dir),
                Data::Number(num) => format!("{}", num),
            }
        }
        pub fn check_types(&self, t: &DataType) -> bool {
            match (self, t) {
                (Data::Thunk(_), _) => true,
                (Data::Number(_), DataType::Number) => true,
            }
        }
    }

    pub type DirData = crate::geom::direction::DirMap<Option<Data>>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Void {}

impl Semigroup for Void {
    fn combine(&self, _: &Self) -> Self {
        match self.clone() {}
    }
}
