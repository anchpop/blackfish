use vek::vec;

pub type Vec2 = vec::Vec2<usize>;
pub type Vec2i = vec::Vec2<i64>;
pub type Extent2 = vec::Extent2<usize>;

pub type Rect = (Vec2, Extent2);

pub mod direction {
    use frunk::monoid::Monoid;
    use frunk::semigroup::Semigroup;

    use super::*;
    use num_traits::ToPrimitive;
    use std::{fmt::Debug, ops::Mul, ops::Neg};
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
        pub const north: Self = Self {
            basis: Basis::North,
            sign: Sign::Positive,
        };
        pub const east: Self = Self {
            basis: Basis::East,
            sign: Sign::Positive,
        };
        pub const south: Self = Self {
            basis: Basis::North,
            sign: Sign::Negative,
        };
        pub const west: Self = Self {
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

        pub fn shift<I: num_traits::WrappingAdd + num_traits::FromPrimitive>(
            &self,
            by: vek::Vec2<I>,
        ) -> vek::Vec2<I> {
            let (x, y) = (by.x, by.y);
            let v = self.to_vector();

            // the addition here should behave correctly,
            // see https://stackoverflow.com/questions/53453628/how-do-i-add-a-signed-integer-to-an-unsigned-integer-in-rust
            // and https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1448b2d8f02f844f72864e10dbe98049
            vek::Vec2::new(
                x.wrapping_add(&num_traits::FromPrimitive::from_i64(v.x).unwrap()),
                y.wrapping_add(&num_traits::FromPrimitive::from_i64(v.y).unwrap()),
            )
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
                0 => Self::north,
                1 => Self::east,
                2 => Self::south,
                3 => Self::west,
                _ => panic!("out of bounds"),
            }
        }

        pub fn rotate(&self, by: Self) -> Self {
            Self::from_num((self.to_num() + by.to_num()) % 4)
        }
        pub fn rotate_vec<I: Neg + std::ops::Neg<Output = I>>(
            &self,
            vec: vec::Vec2<I>,
        ) -> vec::Vec2<I> {
            match self {
                Self {
                    basis: Basis::North,
                    sign: Sign::Positive,
                } => vec::Vec2::new(vec.x, vec.y),
                Self {
                    basis: Basis::East,
                    sign: Sign::Positive,
                } => vec::Vec2::new(vec.y, -vec.x),
                Self {
                    basis: Basis::North,
                    sign: Sign::Negative,
                } => vec::Vec2::new(-vec.x, -vec.y),
                Self {
                    basis: Basis::East,
                    sign: Sign::Negative,
                } => vec::Vec2::new(-vec.y, vec.x),
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
            Self::north
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct GridLine {
        pub location: Vec2i,
        pub side: Basis,
    }
    impl GridLine {
        pub fn new<
            I: num_traits::AsPrimitive<i64> + num_traits::FromPrimitive + num_traits::WrappingAdd,
        >(
            tile: vec::Vec2<I>,
            mut side: Dir,
        ) -> Self {
            if side.sign == Sign::Positive {
                let tile = Vec2i::new(tile.x.as_(), tile.y.as_());
                Self {
                    location: tile,
                    side: side.basis,
                }
            } else {
                let tile = side.shift(tile);
                let side = -side;
                Self::new(tile, side)
            }
        }
    }
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct GridLineDir {
        pub grid_line: GridLine,
        pub direction: Sign,
    }
    impl GridLineDir {
        pub fn parts(self) -> (Vec2i, Dir) {
            (
                self.grid_line.location,
                Dir {
                    basis: self.grid_line.side,
                    sign: self.direction,
                },
            )
        }

        pub fn new<
            I: num_traits::AsPrimitive<i64> + num_traits::FromPrimitive + num_traits::WrappingAdd,
        >(
            tile: vec::Vec2<I>,
            side: Dir,
        ) -> Self {
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

        pub fn rotate(self, dir: &Dir) -> Self {
            Self::deitemize({
                let mut items = self.itemize();
                items.rotate_right(dir.to_num());
                items
            })
        }
    }

    impl<V> IntoIterator for DirMap<V> {
        type Item = (Dir, V);

        type IntoIter = std::vec::IntoIter<Self::Item>;

        fn into_iter(self) -> Self::IntoIter {
            vec![
                (Dir::north, self.north),
                (Dir::east, self.east),
                (Dir::south, self.south),
                (Dir::west, self.west),
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

pub mod tilemap {
    use super::direction::*;
    use super::{Extent2, Vec2, Vec2i};
    use ndarray::{arr2, Array2};
    use nonempty::NonEmpty;
    use proptest::collection::hash_set;
    use slotmap::{new_key_type, Key, SlotMap};
    use std::{
        collections::{HashMap, HashSet},
        iter,
        num::NonZeroUsize,
    };
    pub trait Shaped {
        fn shape(&self) -> NonEmpty<Vec2i>;
    }
    #[derive(Debug, Clone)]
    pub struct Tilemap<K: Key, I: Shaped> {
        pub tiles: SlotMap<K, (Vec2, Dir, I)>,
        pub map: Array2<Option<K>>,
    }

    impl<K: Key, I: PartialEq + Shaped> Tilemap<K, I> {
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

        fn get_tile_positions(
            &self,
            location: &Vec2,
            orientation: &Dir,
            tile: &I,
        ) -> Option<Vec<Vec2>> {
            let shape = tile.shape().into_iter();
            let shape = shape.map(|v| orientation.rotate_vec(v));
            let positions = shape.map(|v| {
                let location: Vec2i = Vec2i::new(location.x as i64, location.y as i64);
                location + v
            });
            let positions = positions.map(|location| self.check_in_bounds_i(location));
            let positions: Option<Vec<Vec2>> = positions.collect();
            positions
        }

        pub fn check_empty(&self, location: Vec2) -> bool {
            self.get_loc(location).is_none()
        }

        pub fn add(mut self, location: Vec2, orientation: Dir, tile: I) -> Result<Self, Self> {
            if let Some(to_place) = self.get_tile_positions(&location, &orientation, &tile) {
                if to_place.iter().all(|to_place| self.check_empty(*to_place)) {
                    let key = self.tiles.insert((location, orientation, tile));
                    for location in to_place {
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
                    let new_tile_positions: HashSet<Vec2> =
                        new_tile_positions.into_iter().collect();

                    let old_tile_positions: HashSet<Vec2> = old_tile
                        .and_then(|old_tile| {
                            self.get_tile_positions(&old_tile.0, &old_tile.1, &old_tile.2)
                        })
                        .map(|positions| positions.into_iter().collect::<HashSet<Vec2>>())
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
                Err(self)
            }
        }

        pub fn remove(mut self, location: Vec2) -> Self {
            if let Some((location, orientation, tile)) = self.get(location) {
                let to_remove = self
                    .get_tile_positions(location, orientation, tile)
                    .expect("Tile should never be invalid while on the board");
                for to_remove in to_remove {
                    self.unset_loc(to_remove)
                }
                todo!()
            } else {
                self
            }
        }

        /// Performs a raycast from the gridline in the specified direction. Returns a GridLineDir representing the location and normal direction of the hit.
        /// (Every raycast will hit something, whether it be the bounds of the map or the bounds or a tile.) Optionally also returns tile info for
        /// if one was hit.
        pub fn raycast(
            &self,
            grid_line_dir: GridLineDir,
        ) -> (GridLineDir, Option<&(Vec2, Dir, I)>) {
            assert!(
                self.check_grid_line_in_bounds(grid_line_dir.grid_line),
                "raycast out of bounds"
            );
            let (location, direction) = grid_line_dir.parts();
            let new_loc = direction.shift(location);
            if let Some(new_loc) = self.check_in_bounds_i(new_loc) {
                if let Some(hit) = self.get(new_loc) {
                    (GridLineDir::new(new_loc, -direction), Some(hit))
                } else {
                    self.raycast(GridLineDir::new(new_loc, direction))
                }
            } else {
                (GridLineDir::new(new_loc, -direction), None)
            }
        }

        pub fn check_in_bounds(&self, location: Vec2) -> bool {
            let location = Vec2::new(location.x as usize, location.y as usize);
            if Vec2::partial_min(location, self.extents()) != location {
                true
            } else {
                false
            }
        }
        pub fn check_in_bounds_i(&self, location: Vec2i) -> Option<Vec2> {
            if !location.is_any_negative() {
                let location = Vec2::new(location.x as usize, location.y as usize);
                if Vec2::partial_min(location, self.extents()) == location {
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

    /*
    struct IterTilemap<'a, K: 'a + Key, I: 'a + Shaped> {
        inner: &'a Tilemap<K, I>,
        // And there is a position used to know where you are in your iteration.
        pos: usize,
    }
    impl<'a, K: 'a + Key, I: 'a + Shaped> Iterator for IterTilemap<'a, K, I> {
        type Item = &'a (Vec2, Dir, I);

        fn next(&mut self) -> Option<Self::Item> {
            if self.pos >= self.inner.0.len() {
                // Obviously, there isn't any more data to read so let's stop here.
                None
            } else {
                // We increment the position of our iterator.
                self.pos += 1;
                // We return the current value pointed by our iterator.
                self.inner.0.get(self.pos - 1)
            }
        }
    } */
}
