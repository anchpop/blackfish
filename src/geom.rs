use std::{fmt::Debug, ops::Neg};
use vek::vec;

pub type Vec2 = vec::Vec2<usize>;
pub type Vec2i = vec::Vec2<i64>;
pub type Extent2 = vec::Extent2<usize>;

pub type Rect = (Vec2, Extent2);

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy, Ord, PartialOrd)]
pub enum Dir {
    North,
    East,
    South,
    West,
}
impl Dir {
    pub fn to_vector(&self) -> Vec2i {
        match self {
            Self::North => Vec2i::new(0, 1),
            Self::East => Vec2i::new(1, 0),
            Self::South => Vec2i::new(0, -1),
            Self::West => Vec2i::new(-1, 0),
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

    pub fn to_num(&self) -> usize {
        match self {
            Self::North => 0,
            Self::East => 1,
            Self::South => 2,
            Self::West => 3,
        }
    }

    pub fn from_num(i: usize) -> Self {
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
    pub fn rotate_vec<I: Neg + std::ops::Neg<Output = I>>(
        &self,
        vec: vec::Vec2<I>,
    ) -> vec::Vec2<I> {
        match self {
            Self::North => vec::Vec2::new(vec.x, vec.y),
            Self::East => vec::Vec2::new(vec.y, -vec.x),
            Self::South => vec::Vec2::new(-vec.x, -vec.y),
            Self::West => vec::Vec2::new(-vec.y, vec.x),
        }
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

pub mod tilemap {
    use super::Vec2;
    use super::Vec2i;
    use super::{Dir, Extent2};
    use ndarray::{arr2, Array2};
    use nonempty::NonEmpty;
    use slotmap::{new_key_type, Key, SlotMap};
    use std::{
        collections::{BTreeMap, HashMap},
        iter,
        num::NonZeroUsize,
    };
    pub trait Shaped {
        fn shape(&self) -> NonEmpty<Vec2i>;
    }
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
            let positions = positions.map(|location| {
                if !location.is_any_negative() {
                    let location = Vec2::new(location.x as usize, location.y as usize);
                    if Vec2::partial_min(location, self.extents()) != location {
                        Some(location)
                    } else {
                        None
                    }
                } else {
                    None
                }
            });
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
}
