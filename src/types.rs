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
pub struct TilePosition(pub Vec2);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TileSize(pub Extent2);

pub struct Materials {
    pub tiles: HashMap<&'static str, Handle<ColorMaterial>>,
    pub empty: Handle<ColorMaterial>,
}

pub mod tiles {
    use crate::geom::tilemap::Shaped;
    use non_empty_collections::index_map::NonEmptyIndexMap;

    use super::data::*;
    use super::*;

    #[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct ProgramInfo {}
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
            ProgramInfo {}
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

    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum BuiltInMachine<D> {
        Iffy(D, D, D),
        Trace(D),
        Produce(D),
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

    #[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum MachineInfo<I> {
        BuiltIn(BuiltInMachine<()>, I),
    }
    #[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

        /// Since tiles are rectangles, this precisely describes each tile's inputs/outputs. The length of the first vector
        /// determines the width and its elements determine the top and bottom inputs/outputs. The length of the second vector
        /// determines the height and its elements determine the left and right inputs/outputs.
        // Positive signs (top/north and right/east come first)
        pub fn block_desc(
            &self,
        ) -> (
            Vec<(Option<IOType>, Option<IOType>)>,
            Vec<(Option<IOType>, Option<IOType>)>,
        ) {
            match self {
                TileProgramF::Machine(machine_info) => match machine_info {
                    MachineInfo::BuiltIn(builtin, _) => match builtin {
                        BuiltInMachine::Iffy(_, _, _) => (
                            vec![(
                                Some(IOType::Out("a".to_owned())),      // north
                                Some(IOType::In("boolean".to_owned())), // south
                            )],
                            vec![(
                                Some(IOType::In("a1".to_owned())), // east
                                Some(IOType::In("a2".to_owned())), //west
                            )],
                        ),
                        BuiltInMachine::Trace(_) => (
                            vec![(
                                None,                                 // north
                                Some(IOType::In("trace".to_owned())), // south
                            )],
                            vec![(
                                None, // east
                                None, //west
                            )],
                        ),
                        BuiltInMachine::Produce(_) => (
                            vec![(
                                Some(IOType::Out("a".to_owned())), // north
                                Some(IOType::In("a".to_owned())),  // south
                            )],
                            vec![(
                                None, // east
                                None, //west
                            )],
                        ),
                    },
                },
            }
        }

        pub fn get_center(&self) -> Vec2i {
            Vec2i::new(0, 0)
        }

        pub fn into_tiles(&self) -> NonEmptyIndexMap<Vec2i, DirMap<Option<IOType>>> {
            match self {
                TileProgramF::Machine(a) => match a {
                    MachineInfo::BuiltIn(_, _) => {
                        let desc = self.block_desc();
                        NonEmptyIndexMap::new(
                            self.get_center(),
                            DirMap {
                                north: desc.0[0].0.clone(),
                                east: desc.1[0].0.clone(),
                                south: desc.0[0].1.clone(),
                                west: desc.1[0].1.clone(),
                            },
                        )
                    }
                },
            }
        }

        pub fn get_inputs(
            m: NonEmptyIndexMap<Vec2, DirMap<Option<IOType>>>,
        ) -> HashMap<MachineInput, GridLineDir> {
            let i = m
                .into_iter()
                .flat_map(|(position, dir_map)| {
                    dir_map
                        .into_iter()
                        .map(|(direction, iotype)| {
                            (GridLineDir::new(position.clone(), direction), iotype)
                        })
                        .collect::<Vec<_>>()
                        .into_iter()
                })
                .filter_map(|(grid_line_dir, iotype)| {
                    if let Some(IOType::In(iotype)) = iotype {
                        Some((iotype, grid_line_dir))
                    } else {
                        None
                    }
                })
                .collect();
            i
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
        fn shape(&self) -> NonEmptyIndexMap<Vec2i, Self::ExtraInfo> {
            self.into_tiles()
        }
    }

    impl tilemap::Shaped for TilePhysics {
        type ExtraInfo = ();
        fn shape(&self) -> NonEmptyIndexMap<Vec2i, Self::ExtraInfo> {
            NonEmptyIndexMap::new(Vec2i::new(0, 0), ()) // assume only one tile at position (0,0)
        }
    }

    impl tilemap::Shaped for TileWorld {
        type ExtraInfo = ();
        fn shape(&self) -> NonEmptyIndexMap<Vec2i, Self::ExtraInfo> {
            match self {
                TileWorld::Phys(t) => t.shape(),
                TileWorld::Prog(t) => {
                    NonEmptyIndexMap::from_iterator(t.shape().into_iter().map(|(loc, _)| (loc, ())))
                        .unwrap()
                }
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
        pub inputs: Vec<(uuid::Uuid, MachineInput, DataType)>,
        pub outputs: Vec<(uuid::Uuid, MachineOutput, DataType)>,
        //pub functions: SlotMap<KeyFunction, TilemapProgram>, // need to make this work with alpha-equivalence
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TilemapWorld {
        pub world: tilemap::Tilemap<KeyWorld, TileWorld>,
        pub inputs: Vec<(MachineInput, Option<Data>)>,
        pub outputs: Vec<(MachineOutput, Option<Data>)>,
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
            F: FnOnce(
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
            F: FnOnce(tilemap::Tilemap<KeyWorld, TileWorld>) -> tilemap::Tilemap<KeyWorld, TileWorld>,
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

        pub fn get_from_map<A, F: FnOnce(&tilemap::Tilemap<KeyWorld, TileWorld>) -> A>(
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
            inputs: Vec<(MachineInput, Option<Data>)>,
            outputs: Vec<(MachineOutput, Option<Data>)>,
            lasers: Vec<TileLineDir>,
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
            let world = TilemapWorld {
                world: tilemap::Tilemap {
                    tiles: world_tiles,
                    map: world_map,
                },
                inputs,
                outputs,
            };
            world
                .try_do_to_map(|mut map| {
                    for laser in lasers {
                        for location in laser.into_iter() {
                            if let Some(location) = map.check_in_bounds_i(location) {
                                map = map.update(location, |tile| match tile {
                                    None => Some((
                                        location,
                                        Dir::default(),
                                        TileWorld::Phys(TilePhysics::Laser(DirMap::empty())),
                                    )),
                                    Some((
                                        location,
                                        orientation,
                                        TileWorld::Phys(TilePhysics::Laser(a)),
                                    )) => Some((
                                        location.clone(),
                                        orientation.clone(),
                                        TileWorld::Phys(TilePhysics::Laser(a.clone())),
                                    )),
                                    a => a.cloned(),
                                })?
                            } else {
                                panic!("Laser goes off past the map!");
                            }
                        }
                    }
                    Ok(map)
                })
                .unwrap()
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
    use crate::geom::Vec2;
    use frunk::monoid::Monoid;
    use frunk::semigroup::Semigroup;

    pub type MachineInput = String;
    pub type MachineOutput = String;

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
    pub enum IOType {
        In(MachineInput),
        Out(MachineOutput),
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum DataType {
        Number,
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum Data {
        Nothing,
        ThunkPure(GraphNode, Dependency),
        ThunkBuiltinOp(Box<BuiltInMachine<Data>>, MachineOutput),
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
                Data::Nothing => format!("nothing"),
                Data::ThunkPure(dep, on) => format!("dep on {:?}'s {:?}", dep, on),
                Data::ThunkBuiltinOp(op, _) => format!("op: {:?}", op.name()),
                Data::Number(num) => format!("{}", num),
            }
        }
        pub fn check_types(&self, t: &DataType) -> bool {
            match (self, t) {
                (Data::Nothing, _) => true,
                (Data::ThunkPure(_, _), _) => true,
                (Data::ThunkBuiltinOp(_, _), _) => true,
                (Data::Number(_), DataType::Number) => true,
            }
        }
    }
    impl From<(GraphNode, FromConnection)> for Data {
        fn from((graph_node, from_connection): (GraphNode, FromConnection)) -> Self {
            Self::ThunkPure(graph_node, Dependency::from(from_connection))
        }
    }

    pub type DirData = crate::geom::direction::DirMap<Option<Data>>;

    #[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum GraphNode {
        Input(uuid::Uuid),
        Output(uuid::Uuid),
        Block((usize, usize), Dir, super::tiles::TileProgram),
        Nothing((i64, i64), Dir),
    }

    impl GraphNode {
        pub fn new(tile_info: (Vec2, Dir, super::tiles::TileProgram)) -> Self {
            Self::Block((tile_info.0.x, tile_info.0.y), tile_info.1, tile_info.2)
        }
        pub fn nothing(grid_line_dir: GridLineDir) -> Self {
            let (location, direction) = grid_line_dir.previous();
            Self::Nothing((location.x, location.y), direction)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum FromConnection {
        GlobalInput(GridLineDir),
        FunctionOutput(GridLineDir, MachineOutput),
        Nothing(GridLineDir),
    }
    impl FromConnection {
        pub fn loc(&self) -> &GridLineDir {
            match self {
                Self::GlobalInput(loc) => loc,
                Self::FunctionOutput(loc, _) => loc,
                Self::Nothing(loc) => loc,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum ToConnection {
        GlobalOutput(GridLineDir),
        FunctionInput(GridLineDir, MachineInput),
        Nothing(GridLineDir),
    }
    impl ToConnection {
        pub fn loc(&self) -> &GridLineDir {
            match self {
                Self::GlobalOutput(loc) => loc,
                Self::FunctionInput(loc, _) => loc,
                Self::Nothing(loc) => loc,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum Dependency {
        On(MachineOutput),
        Only,
    }
    impl From<FromConnection> for Dependency {
        fn from(item: FromConnection) -> Self {
            match item {
                FromConnection::GlobalInput(loc) => Dependency::Only,
                FromConnection::FunctionOutput(loc, output) => Dependency::On(output),
                FromConnection::Nothing(loc) => Dependency::Only,
            }
        }
    }

    pub type GraphEdge = (FromConnection, ToConnection);
    pub type Graph = petgraph::graphmap::GraphMap<GraphNode, GraphEdge, petgraph::Directed>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Void {}

impl Semigroup for Void {
    fn combine(&self, _: &Self) -> Self {
        match self.clone() {}
    }
}
