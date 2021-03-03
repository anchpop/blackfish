use crate::types::*;

use ndarray::arr2;

fn default_map() -> TilemapProgram {
    let mut tiles = TilemapProgram::make_slotmap();
    let north_laser = tiles.insert(TileProgram::LaserProducer(Dir::North, Data::Number(2)));
    let west_laser = tiles.insert(TileProgram::LaserProducer(Dir::West, Data::Number(2)));
    let west_laser_2 = tiles.insert(TileProgram::LaserProducer(Dir::West, Data::Number(2)));

    TilemapProgram(Tilemap {
        tiles,
        map: arr2(&[
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [
                None,
                None,
                None,
                Some(north_laser),
                None,
                Some(west_laser),
                None,
                None,
                None,
            ],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(west_laser_2),
                None,
            ],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
        ]),
    })
}

fn empty_map() -> TilemapProgram {
    let tiles = TilemapProgram::make_slotmap();

    TilemapProgram(Tilemap {
        tiles,
        map: arr2(&[
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
            [None, None, None, None, None, None, None, None, None],
        ]),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    // I want to put algebra-driven design into practice and design a tilemap algebra with proprety-based tests.
    // But for now, I'm just going to write simple unit tests to test things that obviously should work.

    // forall pos tilemap tile. { tilemap.set_tile(pos, tile); tilemap.get(pos) == tile }

    #[cfg(test)]
    mod equality {
        use super::*;

        #[test]
        fn basic_tilemap_equality() {
            assert_eq!(default_map(), default_map());
        }

        #[test]
        fn tilemap_world_equality() {
            assert_eq!(default_map().into_world(), default_map().into_world());
        }

        #[test]
        fn tilemap_equality_invariant_to_unreferenced_tiles() {
            let m1 = default_map();
            let mut m2 = m1.clone();
            m2.0.tiles
                .insert(TileProgram::LaserProducer(Dir::West, Data::Number(2)));
            assert_eq!(m1, m2);
        }

        #[test]
        fn tilemap_equality_alpha_equivalence() {
            let m1 = default_map();
            let mut m2 = m1.clone();
            m2.0.set_tile(
                (3, 2),
                TileProgram::LaserProducer(Dir::North, Data::Number(2)),
            );
            assert_eq!(m1, m2);
        }

        #[test]
        fn tilemap_inequality() {
            let m1 = default_map();
            let mut m2 = m1.clone();
            m2.0.set_tile(
                (0, 0),
                TileProgram::LaserProducer(Dir::North, Data::Number(2)),
            );
            assert_ne!(m1, m2);
        }
    }

    #[cfg(test)]
    mod input_output {
        use super::*;
        use crate::world_sim;

        #[test]
        fn get_inputs() {
            let data = Data::Number(2);
            let map = {
                let mut map = empty_map();
                map.add_tile((3, 1), TileProgram::LaserProducer(Dir::North, data.clone()));
                map
            };

            let world = world_sim::sim(map);
            assert_eq!(world.get_inputs((3, 3), Dir::North).unwrap(), data);
        }
    }
}
