// Board representation: wrapping grid of tile locations.
// Rows wrap around, and each row wraps to the next row.
// It's like a spiral around a torus.
//
// A 4x4 example:
//
// 11  12  13  14  15
//    \ | \ | \ | \ |
// 15 - 0 - 1 - 2 - 3 - 4
//    \ | \ | \ | \ | \
//  3 - 4 - 5 - 6 - 7 - 8
//    \ | \ | \ | \ | \
//  7 - 8 - 9 -10 -11 -12
//    \ | \ | \ | \ | \
// 11 -12 -13 -14 -15 - 0
//       | \ | \ | \ | \
//       0   1   2   3   4

// Index of a board location.
#[cfg(not(feature = "larger-grid"))]
pub(crate) type Id = u8;
#[cfg(feature = "larger-grid")]
pub(crate) type Id = u16;

#[cfg(not(feature = "larger-grid"))]
pub const ROW_SIZE: Id = 16;
#[cfg(feature = "larger-grid")]
pub const ROW_SIZE: Id = 32;

pub(crate) const GRID_SIZE: usize = ROW_SIZE as usize * ROW_SIZE as usize;
pub(crate) const GRID_MASK: Id = (GRID_SIZE as Id).wrapping_sub(1);
// In the middle of the columns and the rows.
// To slightly increase cache locality in the early game,
// and to make formatting slightly simpler.
pub(crate) const START_ID: Id = ROW_SIZE / 2 * (ROW_SIZE + 1);

#[repr(u16)]
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    NW = (GRID_MASK & (ROW_SIZE + 1).wrapping_neg()) as u16,
    NE = (GRID_MASK & ROW_SIZE.wrapping_neg()) as u16,
    W = GRID_MASK as u16 & 1u16.wrapping_neg(),
    E = 1,
    SW = ROW_SIZE as u16,
    SE = ROW_SIZE as u16 + 1,
    None = 0,
}

// In clockwise order
impl Direction {
    pub fn all() -> &'static [Direction; 6] {
        // Omits None.
        &[Direction::NW, Direction::NE, Direction::E, Direction::SE, Direction::SW, Direction::W]
    }

    pub fn apply(self, id: Id) -> Id {
        GRID_MASK & id.wrapping_add(self as Id)
    }
}

pub(crate) fn adjacent(id: Id) -> [Id; 6] {
    // In clockwise order
    [
        Direction::NW.apply(id),
        Direction::NE.apply(id),
        Direction::E.apply(id),
        Direction::SE.apply(id),
        Direction::SW.apply(id),
        Direction::W.apply(id),
    ]
}

// Efficient set utility.
const NODESET_NUM_WORDS: usize = GRID_SIZE / 32;
const NODESET_SHIFT: u32 = GRID_SIZE.trailing_zeros() - 5;
const NODESET_MASK: usize = NODESET_NUM_WORDS - 1;

pub(crate) struct NodeSet {
    table: [u32; NODESET_NUM_WORDS],
}

impl NodeSet {
    pub(crate) fn new() -> NodeSet {
        NodeSet { table: [0; NODESET_NUM_WORDS] }
    }

    pub(crate) fn set(&mut self, id: Id) {
        self.table[id as usize & NODESET_MASK] |= 1 << (id as u32 >> NODESET_SHIFT);
    }

    pub(crate) fn get(&self, id: Id) -> bool {
        (self.table[id as usize & NODESET_MASK] >> (id as u32 >> NODESET_SHIFT)) & 1 != 0
    }
}
