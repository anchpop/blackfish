use std::{fmt::Debug, iter};

use crate::geom::{direction::*, tilemap::RaycastHit, *};

use ndarray::Array2;

use slotmap::{new_key_type, SlotMap};

use frunk::{monoid::Monoid, semigroup::Semigroup};

pub mod tiles {
    use non_empty_collections::index_map::NonEmptyIndexMap;

    use super::{data::*, *};

    use std::collections::HashMap;

    #[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub struct ProgramInfo;
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
    new_key_type! { pub struct KeyNamedConstant; }
    #[derive(Copy, Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum TileProgramF<I> {
        Machine(MachineInfo<I>),
        Literal(KeyNamedConstant),
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
                Self::Literal(_) => "Constant",
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
                                Some(IOType::OutLong("a".to_owned())),  // north
                                Some(IOType::In("boolean".to_owned())), // south
                            )],
                            vec![(
                                Some(IOType::In("a1".to_owned())), // east
                                Some(IOType::In("a2".to_owned())), // west
                            )],
                        ),
                        BuiltInMachine::Trace(_) => (
                            vec![(
                                None,                                 // north
                                Some(IOType::In("trace".to_owned())), // south
                            )],
                            vec![(
                                None, // east
                                None, // west
                            )],
                        ),
                        BuiltInMachine::Produce(_) => (
                            vec![(
                                Some(IOType::OutLong("a".to_owned())), // north
                                Some(IOType::In("a".to_owned())),      // south
                            )],
                            vec![(
                                None, // east
                                None, // west
                            )],
                        ),
                    },
                },
                TileProgramF::Literal(_) => (
                    vec![(
                        Some(IOType::OutShort("n".to_owned())), // north
                        Some(IOType::OutShort("s".to_owned())), // south
                    )],
                    vec![(
                        Some(IOType::OutShort("e".to_owned())), // east
                        Some(IOType::OutShort("w".to_owned())), // west
                    )],
                ),
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
                TileProgramF::Literal(_) => {
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
        pub fn get_outputs(
            m: NonEmptyIndexMap<Vec2, DirMap<Option<IOType>>>,
        ) -> HashMap<(MachineInput, bool), GridLineDir> {
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
                .filter_map(|(grid_line_dir, iotype)| match iotype {
                    Some(IOType::OutLong(iotype)) => Some(((iotype, true), grid_line_dir)),
                    Some(IOType::OutShort(iotype)) => Some(((iotype, false), grid_line_dir)),
                    _ => None,
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
        pub fn size(&self) -> Extent2 {
            match self {
                Self::Phys(TilePhysics::Laser(_)) => Extent2::new(1, 1),
                Self::Prog(TileProgramMachineInfo::Machine(MachineInfo::BuiltIn(b, _))) => {
                    match b {
                        BuiltInMachine::Iffy(_, _, _) => Extent2::new(1, 1),
                        BuiltInMachine::Trace(_) => Extent2::new(1, 1),
                        BuiltInMachine::Produce(_) => Extent2::new(1, 1),
                    }
                }
                Self::Prog(TileProgramMachineInfo::Literal(_)) => Extent2::new(1, 1),
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
                TileProgram::Literal(lit) => TileProgramMachineInfo::Literal(lit),
            }
        }

        pub fn into_world(self) -> TileWorld {
            TileWorld::Prog(self.create_machine_info())
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
    use super::{data::*, tiles::*, *};

    use std::collections::HashSet;

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

    #[derive(Debug, Clone)]
    pub struct TilemapProgram {
        pub spec: tilemap::Tilemap<KeyProgram, TileProgram>,
        pub constants: SlotMap<KeyNamedConstant, NfData>,
        pub inputs: Vec<(uuid::Uuid, MachineInput, DataType)>,
        pub outputs: Vec<(uuid::Uuid, MachineOutput, DataType)>,
        //pub functions: SlotMap<KeyFunction, TilemapProgram>, // need to make this work with alpha-equivalence
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct TilemapWorld {
        pub world: tilemap::Tilemap<KeyWorld, TileWorld>,
        pub inputs: Vec<(MachineInput, Option<Data>)>,
        pub outputs: Vec<(MachineOutput, Data)>,
        pub connection_info: ConnectionInfo,
    }

    impl TilemapWorld {
        pub fn get_input_grid_line_dir(&self, index: InputIndex) -> GridLineDir {
            GridLineDir::new(Vec2i::new(-1, index as i64), Dir::EAST)
        }
        pub fn get_output_grid_line_dir(&self, index: OutputIndex) -> GridLineDir {
            GridLineDir::new(Vec2::new(self.world.extents().w, index), Dir::WEST)
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
                Ok(world) => Ok(Self { world, ..self }),
                Err(world) => Err(Self { world, ..self }),
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
                ..self
            }
        }

        pub fn get_from_map<A, F: FnOnce(&tilemap::Tilemap<KeyWorld, TileWorld>) -> A>(
            self,
            f: F,
        ) -> A {
            f(&self.world)
        }

        pub fn make_slotmap() -> SlotMap<KeyWorld, (Vec2, Dir, TileWorld)> {
            SlotMap::with_key()
        }
    }

    pub type InputIndex = usize;
    pub type OutputIndex = usize;
    impl TilemapProgram {
        pub fn lasercast(
            &self,
            grid_line_dir: GridLineDir,
        ) -> (RaycastHit<TileProgram>, ConnectionPath) {
            let hit = self.spec.raycast(grid_line_dir);
            let path_item = PathItem::Direct(TileLineDir::new(
                grid_line_dir,
                hit.clone().to_normal().grid_line,
            ));
            let path = ConnectionPath(vec![path_item]);
            (hit, path)
        }

        pub fn get_input_grid_line_dir(&self, index: InputIndex) -> GridLineDir {
            GridLineDir::new(Vec2i::new(-1, index as i64), Dir::EAST)
        }
        pub fn get_output_grid_line_dir(&self, index: OutputIndex) -> GridLineDir {
            GridLineDir::new(Vec2::new(self.spec.extents().w, index), Dir::WEST)
        }
        pub fn check_input_grid_line_dir(&self, grid_line_dir: GridLineDir) -> Option<InputIndex> {
            let GridLineDir {
                grid_line,
                direction,
            } = grid_line_dir;
            if grid_line.location.x == -1
                && 0 <= grid_line.location.y
                && (grid_line.location.y as usize) < self.inputs.len()
                && direction == Sign::Positive
                && grid_line.side == Basis::East
            {
                Some(grid_line.location.y as usize)
            } else {
                None
            }
        }
        pub fn check_output_grid_line_dir(
            &self,
            grid_line_dir: GridLineDir,
        ) -> Option<OutputIndex> {
            let GridLineDir {
                grid_line,
                direction,
            } = grid_line_dir;
            if (grid_line.location.x as usize) == self.spec.extents().w - 1
                && 0 <= grid_line.location.y
                && (grid_line.location.y as usize) < self.outputs.len()
                && direction == Sign::Negative
                && grid_line.side == Basis::East
            {
                Some(grid_line.location.y as usize)
            } else {
                None
            }
        }

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
                constants: SlotMap::with_key(),
            }
        }

        pub fn new(tilemap: tilemap::Tilemap<KeyProgram, TileProgram>) -> Self {
            Self {
                spec: tilemap,
                inputs: vec![],
                outputs: vec![],
                constants: SlotMap::with_key(),
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
                Ok(spec) => Ok(Self { spec, ..self }),
                Err(spec) => Err(Self { spec, ..self }),
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
                ..self
            }
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
            outputs: Vec<(MachineOutput, Data)>,
            lasers: HashSet<ConnectionPath>,
            connection_info: ConnectionInfo,
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
                connection_info,
            };
            world
                .try_do_to_map(|mut map| {
                    for laser_path in lasers {
                        for laser_path_item in laser_path.0 {
                            match laser_path_item {
                                PathItem::Direct(laser) => {
                                    for location in laser.into_iter() {
                                        if let Some(location) = map.check_in_bounds_i(location) {
                                            map = map.update(location, |tile| match tile {
                                                None => Some((
                                                    location,
                                                    Dir::default(),
                                                    TileWorld::Phys(TilePhysics::Laser(
                                                        DirMap::empty(),
                                                    )),
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
    use std::{collections::HashMap, ops::Neg};

    use super::{
        tilemaps::{InputIndex, OutputIndex, TilemapProgram},
        tiles::BuiltInMachine,
    };
    use crate::geom::{direction::*, Vec2};
    use frunk::semigroup::Semigroup;

    pub type MachineInput = String;
    pub type MachineOutput = String;

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
    pub enum IOType {
        In(MachineInput),
        OutLong(MachineOutput),
        OutShort(MachineOutput),
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum DataType {
        Number,
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum Data {
        ThunkPure(GraphNode, Dependency),
        ThunkBuiltinOp(Box<BuiltInMachine<Data>>, MachineOutput),
        Whnf(WhnfData),
    }
    type RowLabel = String;
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum WhnfData {
        Nothing,
        Number(i32),
        Product(Vec<(RowLabel, Data)>),
    }
    impl WhnfData {
        pub fn is_nothing(&self) -> bool {
            match self {
                Self::Nothing => true,
                _ => false,
            }
        }
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum NfData {
        Number(i32),
        Product(Vec<(RowLabel, NfData)>),
    }
    impl From<NfData> for WhnfData {
        fn from(data: NfData) -> Self {
            match data {
                NfData::Number(a) => WhnfData::Number(a),
                NfData::Product(l) => WhnfData::Product(
                    l.into_iter()
                        .map(|(label, data)| (label, Data::Whnf(WhnfData::from(data))))
                        .collect(),
                ),
            }
        }
    }

    impl WhnfData {
        pub fn show(&self) -> String {
            match self {
                WhnfData::Nothing => "".to_owned(),
                WhnfData::Number(n) => n.to_string(),
                WhnfData::Product(_) => {
                    todo!()
                }
            }
        }
    }

    impl NfData {
        pub fn show(&self) -> String {
            match self {
                NfData::Number(n) => n.to_string(),
                NfData::Product(_) => {
                    todo!()
                }
            }
        }
    }

    impl Semigroup for Data {
        fn combine(&self, other: &Self) -> Self {
            other.clone()
        }
    }

    impl From<(GraphNode, FromConnection)> for Data {
        fn from((from_node, from_connection): (GraphNode, FromConnection)) -> Self {
            Self::ThunkPure(from_node, Dependency::from(from_connection))
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
        GlobalInput(OutputIndex),
        FunctionOutput(MachineOutput),
        Nothing(),
    }
    impl FromConnection {
        pub fn is_nothing(&self) -> bool {
            match self {
                FromConnection::GlobalInput(_) => false,
                FromConnection::FunctionOutput(_) => false,
                FromConnection::Nothing() => true,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum ToConnection {
        GlobalOutput(InputIndex),
        FunctionInput(MachineInput),
        Nothing(),
    }
    impl ToConnection {
        pub fn is_nothing(&self) -> bool {
            match self {
                ToConnection::GlobalOutput(_) => false,
                ToConnection::FunctionInput(_) => false,
                ToConnection::Nothing() => true,
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
                FromConnection::GlobalInput(_) => Dependency::Only,
                FromConnection::FunctionOutput(output) => Dependency::On(output),
                FromConnection::Nothing() => Dependency::Only,
            }
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum PathItem {
        Direct(TileLineDir),
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct ConnectionPath(pub Vec<PathItem>);

    impl Neg for ConnectionPath {
        type Output = ConnectionPath;

        fn neg(self) -> Self::Output {
            let mut path = self.0;
            path.reverse();
            let path = path.into_iter().map(|item| match item {
                PathItem::Direct(tile_line_dir) => PathItem::Direct(-tile_line_dir),
            });
            Self(path.collect())
        }
    }
    impl ConnectionPath {
        pub fn contains(&self, grid_line_dir: GridLineDir) -> bool {
            self.0.iter().any(|path_item| match path_item {
                PathItem::Direct(tile_line_dir) => {
                    tile_line_dir.contains_grid_line_dir(grid_line_dir)
                }
            })
        }
    }

    pub type GraphEdge = (FromConnection, ToConnection);
    pub type ConnectionInfo = HashMap<(GraphNode, GraphNode, GraphEdge), ConnectionPath>;
    pub type Graph = petgraph::graphmap::GraphMap<GraphNode, GraphEdge, petgraph::Directed>;

    /*
    pub fn graph_edge_to_tile_lines(edge: &GraphEdge, prog: &TilemapProgram) -> Vec<TileLineDir> {
        let (from, to) = edge;
        let from_grid_line_dir = match from {
            FromConnection::GlobalInput(index) => prog.get_input_grid_line_dir(*index),
            FromConnection::FunctionOutput(grid_line_dir, _) => *grid_line_dir,
            FromConnection::Nothing(grid_line_dir) => *grid_line_dir,
        };
        let to_grid_line_dir = match to {
            ToConnection::GlobalOutput(index) => prog.get_output_grid_line_dir(*index),
            ToConnection::FunctionInput(grid_line_dir, _) => *grid_line_dir,
            ToConnection::Nothing(grid_line_dir) => *grid_line_dir,
        };

        vec![TileLineDir::new(
            from_grid_line_dir,
            to_grid_line_dir.grid_line,
        )]
    } */
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Void {}

impl Semigroup for Void {
    fn combine(&self, _: &Self) -> Self {
        match self.clone() {}
    }
}
