#![feature(inherent_associated_types)]
use vek::vec;

pub type Vec2 = vec::Vec2<usize>;
pub type Vec2i = vec::Vec2<i64>;
pub type Extent2 = vec::Extent2<usize>;

pub type Rect = (Vec2, Extent2);

pub mod direction {
    use frunk::{monoid::Monoid, semigroup::Semigroup};

    use super::*;
    use std::{
        fmt::Debug,
        ops::{Mul, Neg},
    };
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
    pub enum Sign {
        Positive,
        Negative,
    }
    impl Neg for Sign {
        type Output = Self;

        fn neg(self) -> Self::Output {
            match self {
                Self::Positive => Self::Negative,
                Self::Negative => Self::Positive,
            }
        }
    }
    impl Mul for Sign {
        type Output = Self;

        fn mul(self, rhs: Self) -> Self::Output {
            if self == rhs {
                Self::Positive
            } else {
                Self::Negative
            }
        }
    }
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
    pub enum Basis {
        North,
        East,
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
    pub struct Dir {
        pub basis: Basis,
        pub sign: Sign,
    }
    impl Dir {
        pub const NORTH: Self = Self {
            basis: Basis::North,
            sign: Sign::Positive,
        };
        pub const EAST: Self = Self {
            basis: Basis::East,
            sign: Sign::Positive,
        };
        pub const SOUTH: Self = Self {
            basis: Basis::North,
            sign: Sign::Negative,
        };
        pub const WEST: Self = Self {
            basis: Basis::East,
            sign: Sign::Negative,
        };

        pub fn to_vector(&self) -> Vec2i {
            match self {
                Self {
                    basis: Basis::North,
                    sign: Sign::Positive,
                } => Vec2i::new(0, 1),
                Self {
                    basis: Basis::East,
                    sign: Sign::Positive,
                } => Vec2i::new(1, 0),
                Self {
                    basis: Basis::North,
                    sign: Sign::Negative,
                } => Vec2i::new(0, -1),
                Self {
                    basis: Basis::East,
                    sign: Sign::Negative,
                } => Vec2i::new(-1, 0),
            }
        }

        pub fn to_arrow(&self) -> &str {
            match self {
                Self {
                    basis: Basis::North,
                    sign: Sign::Positive,
                } => "↑",
                Self {
                    basis: Basis::East,
                    sign: Sign::Positive,
                } => "→",
                Self {
                    basis: Basis::North,
                    sign: Sign::Negative,
                } => "↓",
                Self {
                    basis: Basis::East,
                    sign: Sign::Negative,
                } => "←",
            }
        }

        pub fn shift<I: 'static + num_traits::WrappingAdd + Copy>(
            &self,
            by: vek::Vec2<I>,
        ) -> vek::Vec2<I>
        where
            i64: num_traits::AsPrimitive<I>,
        {
            use num_traits::AsPrimitive;

            let (x, y) = (by.x, by.y);
            let v = self.to_vector();

            // the addition here should behave correctly,
            // see https://stackoverflow.com/questions/53453628/how-do-i-add-a-signed-integer-to-an-unsigned-integer-in-rust
            // and https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1448b2d8f02f844f72864e10dbe98049
            vek::Vec2::new(x.wrapping_add(&(v.x.as_())), y.wrapping_add(&(v.y.as_())))
        }

        pub fn to_num(&self) -> usize {
            match self {
                Self {
                    basis: Basis::North,
                    sign: Sign::Positive,
                } => 0,
                Self {
                    basis: Basis::East,
                    sign: Sign::Positive,
                } => 1,
                Self {
                    basis: Basis::North,
                    sign: Sign::Negative,
                } => 2,
                Self {
                    basis: Basis::East,
                    sign: Sign::Negative,
                } => 3,
            }
        }

        pub fn from_num(i: usize) -> Self {
            match i {
                0 => Self::NORTH,
                1 => Self::EAST,
                2 => Self::SOUTH,
                3 => Self::WEST,
                _ => panic!("out of bounds"),
            }
        }
    }
    impl Neg for Dir {
        fn neg(self) -> Self::Output {
            Dir {
                basis: self.basis,
                sign: -self.sign,
            }
        }

        type Output = Self;
    }
    impl Default for Dir {
        fn default() -> Self {
            Self::NORTH
        }
    }

    pub trait Rotatable {
        fn rotate(self, dir: &Dir) -> Self;
    }

    impl Rotatable for Dir {
        fn rotate(self, by: &Dir) -> Self {
            Self::from_num((self.to_num() + by.to_num()) % 4)
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct GridLine {
        pub location: Vec2i,
        pub side: Basis,
    }
    impl GridLine {
        pub fn new<I: Copy + num_traits::WrappingAdd + num_traits::AsPrimitive<i64>>(
            tile: vec::Vec2<I>,
            side: Dir,
        ) -> Self
        where
            i64: num_traits::AsPrimitive<I>,
        {
            if side.sign == Sign::Positive {
                let tile = Vec2i::new(tile.x.as_(), tile.y.as_());
                Self {
                    location: tile,
                    side: side.basis,
                }
            } else {
                let tile = side.shift(tile);
                let side = -side;

                let tile = Vec2i::new(tile.x.as_(), tile.y.as_());
                Self {
                    location: tile,
                    side: side.basis,
                }
            }
        }

        pub fn distance(&self, other: &Self) -> usize {
            assert_eq!(self.side, other.side);

            if self.location.x == other.location.x {
                (self.location.y as i64 - other.location.y as i64).abs() as usize
            } else if self.location.y == other.location.y {
                (self.location.x as i64 - other.location.x as i64).abs() as usize
            } else {
                panic!("to calculate a distance between grid lines, they need to be on the same column or row!")
            }
        }
    }
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct GridLineDir {
        pub grid_line: GridLine,
        pub direction: Sign,
    }
    impl GridLineDir {
        pub fn previous(self) -> (Vec2i, Dir) {
            if self.direction == Sign::Positive {
                (
                    self.grid_line.location,
                    Dir {
                        basis: self.grid_line.side,
                        sign: self.direction,
                    },
                )
            } else {
                let dir = Dir {
                    basis: self.grid_line.side,
                    sign: self.direction,
                };
                ((-dir).shift(self.grid_line.location), dir)
            }
        }
        pub fn next(self) -> (Vec2i, Dir) {
            let (pos, dir) = self.previous();
            return (dir.shift(pos), dir);
        }
        pub fn advance(self, dist: i64) -> GridLineDir {
            let (mut pos, dir) = self.previous();
            let dir = if dist < 0 { -dir } else { dir };
            let dist = dist.abs();

            for _i in 0..dist {
                pos = dir.shift(pos);
            }
            GridLineDir::new(pos, dir)
        }

        pub fn new<
            I: num_traits::AsPrimitive<i64> + num_traits::FromPrimitive + num_traits::WrappingAdd,
        >(
            tile: vec::Vec2<I>,
            side: Dir,
        ) -> Self
        where
            i64: num_traits::AsPrimitive<I>,
        {
            let grid_line = GridLine::new(tile, side);
            Self {
                grid_line,
                direction: side.sign,
            }
        }

        pub fn new_from_gridline(grid_line: GridLine, direction: Dir) -> Self {
            Self {
                grid_line: grid_line,
                direction: direction.sign,
            }
        }
    }
    impl Neg for GridLineDir {
        type Output = GridLineDir;

        fn neg(self) -> Self::Output {
            Self {
                grid_line: self.grid_line,
                direction: -self.direction,
            }
        }
    }
    impl PartialOrd for GridLineDir {
        fn partial_cmp(&self, other: &Self) -> std::option::Option<std::cmp::Ordering> {
            let (p1, p2) = self.previous();
            let (o1, o2) = other.previous();
            (p1.x, p1.y, p2).partial_cmp(&(o1.x, o1.y, o2))
        }
    }
    impl Ord for GridLineDir {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.partial_cmp(other).unwrap()
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
                Dir {
                    basis: Basis::North,
                    sign: Sign::Positive,
                } => &self.north,
                Dir {
                    basis: Basis::North,
                    sign: Sign::Negative,
                } => &self.south,
                Dir {
                    basis: Basis::East,
                    sign: Sign::Positive,
                } => &self.east,
                Dir {
                    basis: Basis::East,
                    sign: Sign::Negative,
                } => &self.west,
            }
        }
        pub fn update(self, dir: &Dir, v: V) -> Self {
            match dir {
                Dir {
                    basis: Basis::North,
                    sign: Sign::Positive,
                } => Self { north: v, ..self },
                Dir {
                    basis: Basis::North,
                    sign: Sign::Negative,
                } => Self { south: v, ..self },
                Dir {
                    basis: Basis::East,
                    sign: Sign::Positive,
                } => Self { east: v, ..self },
                Dir {
                    basis: Basis::East,
                    sign: Sign::Negative,
                } => Self { west: v, ..self },
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
    }

    impl<V> IntoIterator for DirMap<V> {
        type Item = (Dir, V);

        type IntoIter = std::vec::IntoIter<Self::Item>;

        fn into_iter(self) -> Self::IntoIter {
            vec![
                (Dir::NORTH, self.north),
                (Dir::EAST, self.east),
                (Dir::SOUTH, self.south),
                (Dir::WEST, self.west),
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

    impl<V> Rotatable for DirMap<V> {
        fn rotate(self, by: &Dir) -> Self {
            Self::deitemize({
                let mut items = self.itemize();
                items.rotate_right(by.to_num());
                items
            })
        }
    }
    impl<I: Neg + std::ops::Neg<Output = I>> Rotatable for vek::Vec2<I> {
        fn rotate(self, by: &Dir) -> Self {
            match by {
                Dir {
                    basis: Basis::North,
                    sign: Sign::Positive,
                } => Self::new(self.x, self.y),
                Dir {
                    basis: Basis::East,
                    sign: Sign::Positive,
                } => Self::new(self.y, -self.x),
                Dir {
                    basis: Basis::North,
                    sign: Sign::Negative,
                } => Self::new(-self.x, -self.y),
                Dir {
                    basis: Basis::East,
                    sign: Sign::Negative,
                } => Self::new(-self.y, self.x),
            }
        }
    }

    impl Rotatable for () {
        fn rotate(self, _by: &Dir) -> Self {
            ()
        }
    }
    impl<T1: Rotatable> Rotatable for (T1,) {
        fn rotate(self, by: &Dir) -> Self {
            (self.0.rotate(by),)
        }
    }

    impl<T1: Rotatable, T2: Rotatable> Rotatable for (T1, T2) {
        fn rotate(self, by: &Dir) -> Self {
            (self.0.rotate(by), self.1.rotate(by))
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct TileLine {
        pub grid_line: GridLine,
        pub distance: usize,
    }

    impl TileLine {
        pub fn contains(&self, location: Vec2i) -> bool {
            (0..self.distance).any(|i| {
                location
                    == (GridLineDir {
                        grid_line: self.grid_line,
                        direction: Sign::Positive,
                    })
                    .advance(i as i64)
                    .next()
                    .0
            })
        }

        pub fn contains_grid_line(&self, grid_line: GridLine) -> bool {
            (0..self.distance + 1).any(|i| {
                grid_line
                    == (GridLineDir {
                        grid_line: self.grid_line,
                        direction: Sign::Positive,
                    })
                    .advance(i as i64)
                    .grid_line
            })
        }
    }

    impl TileLine {
        pub fn new(from: GridLine, to: GridLine) -> Self {
            TileLineDir::new(from, to).tile_line
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct TileLineDir {
        pub tile_line: TileLine,
        pub sign: Sign,
    }

    impl TileLineDir {
        pub fn new(from: GridLineDir, to: GridLine) -> Self {
            let distance = from.grid_line.distance(&to);
            if distance == 0 {
                assert_eq!(from.grid_line, to);
                TileLineDir {
                    tile_line: TileLine {
                        grid_line: from.grid_line,
                        distance: 0,
                    },
                    sign: from.direction,
                }
            } else {
                let from = from.grid_line;

                let positive = if from.side == Basis::North {
                    to.location.y > from.location.y
                } else {
                    to.location.x > from.location.x
                };

                let lower = if from.side == Basis::North {
                    Vec2i::new(
                        from.location.x,
                        if positive {
                            from.location.y
                        } else {
                            to.location.y
                        },
                    )
                } else {
                    Vec2i::new(
                        if positive {
                            from.location.x
                        } else {
                            to.location.x
                        },
                        from.location.y,
                    )
                };
                TileLineDir {
                    tile_line: TileLine {
                        grid_line: GridLine {
                            location: lower,
                            side: from.side,
                        },
                        distance,
                    },
                    sign: if positive {
                        Sign::Positive
                    } else {
                        Sign::Negative
                    },
                }
            }
        }

        pub fn get_start(&self) -> GridLineDir {
            self.get_parts().0
        }
        pub fn get_end(&self) -> GridLineDir {
            self.get_parts().1
        }
        pub fn get_parts(&self) -> (GridLineDir, GridLineDir) {
            let dir = Dir {
                basis: self.tile_line.grid_line.side,
                sign: self.sign,
            };
            let unordered_parts = (
                GridLineDir {
                    grid_line: self.tile_line.grid_line,
                    direction: Sign::Positive,
                },
                GridLineDir {
                    grid_line: GridLine::new(
                        self.tile_line.grid_line.location
                            + dir.to_vector() * (self.tile_line.distance as i64),
                        dir,
                    ),
                    direction: Sign::Negative,
                },
            );
            if self.sign == Sign::Positive {
                unordered_parts
            } else {
                (unordered_parts.1, unordered_parts.0)
            }
        }

        pub fn into_iter(self) -> std::vec::IntoIter<vek::Vec2<i64>> {
            let mut v = vec![];
            let starting_dir = self.get_start();
            for i in 0..self.tile_line.distance {
                v.push(starting_dir.advance(i as i64).next().0)
            }
            v.into_iter()
        }

        pub fn contains_grid_line_dir(&self, grid_line_dir: GridLineDir) -> bool {
            self.sign == grid_line_dir.direction
                && self.tile_line.contains_grid_line(grid_line_dir.grid_line)
        }
    }

    impl Neg for TileLineDir {
        fn neg(self) -> Self::Output {
            Self {
                tile_line: self.tile_line,
                sign: -self.sign,
            }
        }

        type Output = TileLineDir;
    }
}

pub mod tilemap {
    use super::{direction::*, Extent2, Vec2, Vec2i};
    use ndarray::Array2;
    use non_empty_collections::index_map::NonEmptyIndexMap;
    use slotmap::{Key, SlotMap};
    use std::collections::{HashMap, HashSet};

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum RaycastHit<I> {
        HitBorder(GridLineDir),
        HitTile(Vec2, Dir, (Vec2, Dir, I)),
    }
    impl<'a, I> RaycastHit<I> {
        pub fn to_normal(self) -> GridLineDir {
            match self {
                RaycastHit::HitBorder(normal) => normal,
                RaycastHit::HitTile(location, normal_direction, _) => {
                    GridLineDir::new(location, normal_direction)
                }
            }
        }
    }

    pub trait Shaped {
        type ExtraInfo: Rotatable;
        fn shape(&self) -> NonEmptyIndexMap<Vec2i, Self::ExtraInfo>;
    }
    #[derive(Debug, Clone)]
    pub struct Tilemap<K: Key, I: Shaped> {
        pub tiles: SlotMap<K, (Vec2, Dir, I)>,
        pub map: Array2<Option<K>>,
    }
    impl<K: Key, I: Shaped> Tilemap<K, I> {
        type TileInfo = (Vec2, Dir, I); // cannot actually use these yet
    }

    impl<K: Key, I: PartialEq + Shaped + Clone> Tilemap<K, I> {
        pub fn extents(&self) -> Extent2 {
            let dim = self.map.dim();
            Extent2::new(dim.1, dim.0)
        }

        pub fn get(&self, location: Vec2) -> Option<&(Vec2, Dir, I)> {
            if let Some(tile_key) = self.get_loc(location) {
                self.tiles.get(tile_key)
            } else {
                None
            }
        }
        pub fn get_unchecked(&self, location: Vec2) -> &(Vec2, Dir, I) {
            self.get(location).unwrap_or_else(|| {
                panic!(
                    "Attempted to access a tile at ({}, {}), but that tile is empty",
                    location.x, location.y
                )
            })
        }
        pub fn get_loc(&self, location: Vec2) -> Option<K> {
            let key = self.map.get([location.y, location.x]).cloned().flatten();
            /*.unwrap_or_else(|| {
                panic!(
                    "Attempted to access a tile at ({}, {}) but it was out of bounds.",
                    location.x, location.y
                )
            }); */
            key
        }
        pub fn set_loc(&mut self, location: Vec2, key: K) {
            self.map[[location.y, location.x]] = Some(key)
        }
        pub fn unset_loc(&mut self, location: Vec2) {
            self.map[[location.y, location.x]] = None
        }

        pub fn get_tile_positions(
            &self,
            location: &Vec2,
            orientation: &Dir,
            tile: &I,
        ) -> Option<NonEmptyIndexMap<Vec2, I::ExtraInfo>> {
            let shape = tile.shape().into_iter();
            let shape = shape.map(|v| v.rotate(orientation));
            let positions = shape.map(|(vec, extra)| {
                let location: Vec2i = Vec2i::new(location.x as i64, location.y as i64);
                (location + vec, extra)
            });
            let positions = positions.map(|(location, extra)| {
                self.check_in_bounds_i(location)
                    .map(|location| (location, extra))
            });
            let positions: Option<Vec<(Vec2, I::ExtraInfo)>> = positions.collect();
            let positions = positions
                .map(|positions| NonEmptyIndexMap::from_iterator(positions.into_iter()).unwrap());
            positions
        }

        pub fn check_empty(&self, location: Vec2) -> bool {
            self.get_loc(location).is_none()
        }

        pub fn add(mut self, location: Vec2, orientation: Dir, tile: I) -> Result<Self, Self> {
            if let Some(to_place) = self.get_tile_positions(&location, &orientation, &tile) {
                if to_place
                    .iter()
                    .all(|(location, _)| self.check_empty(*location))
                {
                    let key = self.tiles.insert((location, orientation, tile));
                    for (location, _) in to_place {
                        self.set_loc(location, key)
                    }
                    Ok(self)
                } else {
                    Err(self)
                }
            } else {
                Err(self)
            }
        }

        pub fn update<F: Fn(Option<&(Vec2, Dir, I)>) -> Option<(Vec2, Dir, I)>>(
            self,
            location: Vec2,
            f: F,
        ) -> Result<Self, Self> {
            let old_tile = self.get(location);

            let new_tile = f(old_tile);
            if let Some(new_tile) = new_tile {
                if let Some(new_tile_positions) =
                    self.get_tile_positions(&new_tile.0, &new_tile.1, &new_tile.2)
                {
                    let new_tile_positions: HashSet<Vec2> = new_tile_positions
                        .into_iter()
                        .map(|(location, _)| location)
                        .collect();

                    let old_tile_positions: HashSet<Vec2> = old_tile
                        .and_then(|old_tile| {
                            self.get_tile_positions(&old_tile.0, &old_tile.1, &old_tile.2)
                        })
                        .map(|positions| {
                            positions
                                .into_iter()
                                .map(|(location, _)| location)
                                .collect::<HashSet<Vec2>>()
                        })
                        .unwrap_or(HashSet::new());

                    let changed_tiles = new_tile_positions.difference(&old_tile_positions);
                    if changed_tiles
                        .clone()
                        .into_iter()
                        .all(|location| self.check_empty(location.clone()))
                    {
                        self.remove(location)
                            .add(new_tile.0, new_tile.1, new_tile.2)
                    } else {
                        Err(self)
                    }
                } else {
                    Err(self)
                }
            } else {
                Ok(self)
            }
        }

        pub fn remove(mut self, location: Vec2) -> Self {
            if let Some(k) = self.get_loc(location) {
                if let Some((location, orientation, tile)) = self.tiles.get(k) {
                    let to_remove = self
                        .get_tile_positions(location, orientation, tile)
                        .expect("Tile should never be invalid while on the board");

                    for (location, _) in to_remove {
                        self.unset_loc(location)
                    }
                    self.tiles.remove(k);
                    self
                } else {
                    self
                }
            } else {
                self
            }
        }

        /// Performs a raycast from the gridline in the specified direction. Returns a GridLineDir representing the location and normal direction of the hit.
        /// (Every raycast will hit something, whether it be the bounds of the map or the bounds or a tile.) Optionally also returns tile info for
        /// if one was hit.
        pub fn raycast(&self, grid_line_dir: GridLineDir) -> RaycastHit<I> {
            assert!(
                self.check_grid_line_in_bounds(grid_line_dir.grid_line),
                "raycast out of bounds"
            );
            let (_old_location, _) = grid_line_dir.previous();
            let (new_location, direction) = grid_line_dir.next();
            if let Some(new_location) = self.check_in_bounds_i(new_location) {
                if let Some(hit) = self.get(new_location) {
                    let hit = hit.clone();
                    RaycastHit::HitTile(new_location, -direction, hit)
                } else {
                    self.raycast(GridLineDir::new(new_location, direction))
                }
            } else {
                RaycastHit::HitBorder(GridLineDir::new(new_location, -direction))
            }
        }

        pub fn check_in_bounds(&self, location: Vec2) -> bool {
            let extents = self.extents();
            if location.x < extents.w && location.y < extents.h {
                true
            } else {
                false
            }
        }
        pub fn check_in_bounds_i(&self, location: Vec2i) -> Option<Vec2> {
            let extents = self.extents();
            if !location.is_any_negative() {
                let location = Vec2::new(location.x as usize, location.y as usize);
                if location.x < extents.w && location.y < extents.h {
                    Some(location)
                } else {
                    None
                }
            } else {
                None
            }
        }

        pub fn check_grid_line_in_bounds(&self, grid_line: GridLine) -> bool {
            if self.check_in_bounds_i(grid_line.location).is_some() {
                true
            } else if (grid_line.location.x == -1) != (grid_line.location.y == -1) {
                if (grid_line.location.x == -1) && (grid_line.side == Basis::East) {
                    true
                } else if (grid_line.location.y == -1) && (grid_line.side == Basis::North) {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
    }

    impl<K: Key, I: PartialEq + Shaped> PartialEq for Tilemap<K, I> {
        // should return true if self and other are alpha-equivalent
        fn eq(&self, other: &Self) -> bool {
            // There's actually no reason that the two Tilemaps being compared need to have the same key type.
            // PartialEq requires they be, but we can write this interior function that doesn't have that restriction
            // and just call it. This prevents us from making bugs where we use the second tilemap key when we should have
            // used the first.
            fn e<Kp: Key, Kpp: Key, Ip: PartialEq + Shaped>(
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
    impl<K: Key, I: Eq + Shaped> Eq for Tilemap<K, I> {}
}
