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
    pub enum BuiltInMachine<D> {
        Iffy(D, D, D),
        Trace(D),
        Produce(D),
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
    pub enum IOType {
        In(String),
        Out(String),
    }

    impl<D> BuiltInMachine<D> {
        /*
        pub fn io(&self) -> BTreeMap<Dir, IOType> {
            match self {
                BuiltInMachine::Iffy => {
                    todo!()
                }
                BuiltInMachine::Trace => {
                    btree_map! {
                        Dir::south: IOType::In("observe".to_string())
                    }
                }
                BuiltInMachine::Produce => {
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
         */

        pub fn name(&self) -> &str {
            match self {
                Self::Produce(_) => "id",
                Self::Iffy(_, _, _) => "if",
                Self::Trace(_) => "trace",
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum MachineInfo<I> {
        BuiltIn(BuiltInMachine<()>, I),
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum TileProgramF<I> {
        Machine(MachineInfo<I>),
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
                Self::Machine(MachineInfo::BuiltIn(BuiltInMachine::Iffy(_, _, _), _)) => "Iffy",
                Self::Machine(MachineInfo::BuiltIn(BuiltInMachine::Trace(_), _)) => "Trace",
                Self::Machine(MachineInfo::BuiltIn(BuiltInMachine::Produce(_), _)) => "Producer",
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
                Self::Prog(TileProgramMachineInfo::Machine(MachineInfo::BuiltIn(b, _))) => {
                    match b {
                        BuiltInMachine::Iffy(_, _, _) => Vec2::new(1, 1),
                        BuiltInMachine::Trace(_) => Vec2::new(1, 1),
                        BuiltInMachine::Produce(_) => Vec2::new(1, 1),
                    }
                }
            }
        }
    }
    impl TileProgram {
        pub fn create_machine_info(self) -> TileProgramMachineInfo {
            match self {
                TileProgram::Machine(machine_info) => {
                    TileProgramMachineInfo::Machine(match machine_info {
                        MachineInfo::BuiltIn(machine_type, info) => MachineInfo::BuiltIn(
                            machine_type,
                            WorldMachineInfo {
                                program_info: info,
                                ..WorldMachineInfo::empty()
                            },
                        ),
                    })
                }
            }
        }
    }

    impl<T> tilemap::Shaped for TileProgramF<T> {
        type ExtraInfo = DirMap<Option<IOType>>;
        fn shape(&self) -> nonempty::NonEmpty<(Vec2i, Self::ExtraInfo)> {
            match self {
                TileProgramF::Machine(a) => match a {
                    MachineInfo::BuiltIn(builtin, _) => match builtin {
                        BuiltInMachine::Iffy(_, _, _) => nonempty::NonEmpty::new((
                            Vec2i::new(0, 0),
                            DirMap {
                                north: Some(IOType::Out("output".to_owned())),
                                east: Some(IOType::In("a".to_owned())),
                                south: Some(IOType::In("boolean".to_owned())),
                                west: Some(IOType::In("b".to_owned())),
                            },
                        )),
                        BuiltInMachine::Trace(_) => nonempty::NonEmpty::new((
                            Vec2i::new(0, 0),
                            DirMap {
                                north: None,
                                east: None,
                                south: Some(IOType::In("trace".to_owned())),
                                west: None,
                            },
                        )),
                        BuiltInMachine::Produce(_) => nonempty::NonEmpty::new((
                            Vec2i::new(0, 0),
                            DirMap {
                                north: Some(IOType::Out("output".to_owned())),
                                east: None,
                                south: Some(IOType::In("trace".to_owned())),
                                west: None,
                            },
                        )),
                    },
                },
            }
        }
    }

    impl tilemap::Shaped for TilePhysics {
        type ExtraInfo = ();
        fn shape(&self) -> nonempty::NonEmpty<(Vec2i, Self::ExtraInfo)> {
            nonempty::NonEmpty::new((Vec2i::new(0, 0), ())) // assume only one tile at position (0,0)
        }
    }

    impl tilemap::Shaped for TileWorld {
        type ExtraInfo = ();
        fn shape(&self) -> nonempty::NonEmpty<(Vec2i, Self::ExtraInfo)> {
            match self {
                TileWorld::Phys(t) => t.shape(),
                TileWorld::Prog(t) => t.shape().map(|v| (v.0, ())),
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
        pub world: tilemap::Tilemap<KeyWorld, TileWorld>,
        pub inputs: Vec<(String, Option<Data>)>,
        pub outputs: Vec<(String, Option<Data>)>,
    }

    impl TilemapWorld {
        #[allow(dead_code)]
        pub fn world(self) -> Array2<Option<KeyWorld>> {
            self.world.map
        }
        pub fn world_dim(&self) -> Extent2 {
            let dim = self.world.map.dim();
            Extent2::new(dim.1, dim.0)
        }
        pub fn try_do_to_map<
            F: Fn(
                tilemap::Tilemap<KeyWorld, TileWorld>,
            ) -> Result<
                tilemap::Tilemap<KeyWorld, TileWorld>,
                tilemap::Tilemap<KeyWorld, TileWorld>,
            >,
        >(
            self,
            f: F,
        ) -> Result<Self, Self> {
            match f(self.world) {
                Ok(world) => Ok(Self {
                    world,
                    inputs: self.inputs,
                    outputs: self.outputs,
                }),
                Err(world) => Err(Self {
                    world,
                    inputs: self.inputs,
                    outputs: self.outputs,
                }),
            }
        }
        pub fn apply_to_map<
            F: Fn(tilemap::Tilemap<KeyWorld, TileWorld>) -> tilemap::Tilemap<KeyWorld, TileWorld>,
        >(
            self,
            f: F,
        ) -> Self {
            Self {
                world: f(self.world),
                inputs: self.inputs,
                outputs: self.outputs,
            }
        }

        pub fn get_from_map<A, F: Fn(&tilemap::Tilemap<KeyWorld, TileWorld>) -> A>(
            self,
            f: F,
        ) -> A {
            f(&self.world)
        }

        /*
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
        }*/

        pub fn make_slotmap() -> SlotMap<KeyWorld, (Vec2, Dir, TileWorld)> {
            SlotMap::with_key()
        }
    }

    impl TilemapProgram {
        pub fn new_empty(dim: Extent2) -> Self {
            let map: Array2<Option<KeyProgram>> =
                Array2::zeros((dim.h, dim.w)).mapv(|_: usize| None);

            Self {
                spec: tilemap::Tilemap {
                    tiles: Self::make_slotmap(),
                    map: map,
                },
                inputs: vec![],
                outputs: vec![],
            }
        }

        pub fn new(tilemap: tilemap::Tilemap<KeyProgram, TileProgram>) -> Self {
            Self {
                spec: tilemap,
                inputs: vec![],
                outputs: vec![],
            }
        }

        pub fn try_do_to_map<
            F: Fn(
                tilemap::Tilemap<KeyProgram, TileProgram>,
            ) -> Result<
                tilemap::Tilemap<KeyProgram, TileProgram>,
                tilemap::Tilemap<KeyProgram, TileProgram>,
            >,
        >(
            self,
            f: F,
        ) -> Result<Self, Self> {
            match f(self.spec) {
                Ok(spec) => Ok(Self {
                    spec,
                    inputs: self.inputs,
                    outputs: self.outputs,
                }),
                Err(spec) => Err(Self {
                    spec,
                    inputs: self.inputs,
                    outputs: self.outputs,
                }),
            }
        }
        pub fn apply_to_map<
            F: Fn(
                tilemap::Tilemap<KeyProgram, TileProgram>,
            ) -> tilemap::Tilemap<KeyProgram, TileProgram>,
        >(
            self,
            f: F,
        ) -> Self {
            Self {
                spec: f(self.spec),
                inputs: self.inputs,
                outputs: self.outputs,
            }
        }

        pub fn get_from_map<A, F: Fn(&tilemap::Tilemap<KeyProgram, TileProgram>) -> A>(
            self,
            f: F,
        ) -> A {
            f(&self.spec)
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
        pub fn into_world(
            self,
            inputs: Vec<(String, Option<Data>)>,
            outputs: Vec<(String, Option<Data>)>,
        ) -> TilemapWorld {
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
                world: tilemap::Tilemap {
                    tiles: world_tiles,
                    map: world_map,
                },
                inputs,
                outputs,
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
    use super::tiles::BuiltInMachine;
    use crate::geom::direction::*;
    use frunk::monoid::Monoid;
    use frunk::semigroup::Semigroup;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum DataType {
        Number,
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Data {
        Nothing(GridLineDir),
        ThunkPure(GridLineDir),
        ThunkBuiltinOp(Box<BuiltInMachine<Data>>),
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
                Data::Nothing(hit) => format!("nothing: {:?}", hit),
                Data::ThunkPure(dep) => format!("thunk: {:?}", dep),
                Data::ThunkBuiltinOp(op) => format!("op: {:?}", op.name()),
                Data::Number(num) => format!("{}", num),
            }
        }
        pub fn check_types(&self, t: &DataType) -> bool {
            match (self, t) {
                (Data::Nothing(_), _) => true,
                (Data::ThunkPure(_), _) => true,
                (Data::ThunkBuiltinOp(_), _) => true,
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
