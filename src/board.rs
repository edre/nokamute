extern crate minimax;

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::default::Default;
use std::fmt::{Display, Formatter, Result};

// Ideas for board representation:
// 1) Grid based: Keep a mostly empty grid with entries for what's in each cell.
//      The grid will need to expand and/or translate if the hive gets too long or moves.
// 2) Graph based: Each piece points to its neighbors.
//      Recalculating connectedness seems complex.
//      Even computing adjacent nodes may require walking all the way through the other pieces...
// 3) Location based: Hashmap or other association from coordinates to piece.
//      Don't need to resize the grid, doesn't take more space than necessary.

// Location-based model.
// Hex coordinates. Grid connections plus one of the diagonals. First bug is at (0,0).
pub type Loc = (i8, i8);

fn adjacent(loc: Loc) -> [Loc; 6] {
    let (x, y) = loc;
    [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y), (x - 1, y - 1), (x + 1, y + 1)]
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Bug {
    Queen,
    Grasshopper,
    Spider,
    Ant,
    Beetle,
}

impl Bug {
    fn index(&self) -> usize {
        match *self {
            Bug::Queen => 0,
            Bug::Grasshopper => 1,
            Bug::Spider => 2,
            Bug::Ant => 3,
            Bug::Beetle => 4,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Color {
    Black,
    White,
}

// A tile on the board.
struct Tile {
    bug: Bug,
    color: Color,
    underneath: Option<Box<Tile>>,
}

pub struct Board {
    // TODO: try some simpler association list.
    grid: HashMap<Loc, Tile>,
    remaining: [[u8; 5]; 2],
    move_num: u16,
}

impl Board {
    fn to_move(&self) -> Color {
        if self.move_num % 2 == 0 {
            Color::Black
        } else {
            Color::White
        }
    }

    fn get(&self, loc: Loc) -> Option<&Tile> {
        self.grid.get(&loc)
    }

    fn insert(&mut self, loc: Loc, bug: Bug, color: Color) {
        if let Some(prev) = self.grid.insert(loc, Tile { bug: bug, color: color, underneath: None })
        {
            self.grid.get_mut(&loc).unwrap().underneath = Some(Box::new(prev));
        }
    }

    // Asserts that there is something there.
    fn remove(&mut self, loc: Loc) -> Tile {
        let mut tile = self.grid.remove(&loc).unwrap();
        if let Some(stack) = tile.underneath.take() {
            self.grid.insert(loc, *stack);
        }
        tile
    }

    fn get_remaining(&self) -> &[u8; 5] {
        &self.remaining[self.move_num as usize & 1]
    }

    fn mut_remaining(&mut self) -> &mut [u8; 5] {
        &mut self.remaining[self.move_num as usize & 1]
    }

    fn get_available_bugs(&self) -> [(Bug, u8); 5] {
        let remaining = self.get_remaining();
        [
            (Bug::Queen, remaining[0]),
            (Bug::Grasshopper, remaining[1]),
            (Bug::Spider, remaining[2]),
            (Bug::Ant, remaining[3]),
            (Bug::Beetle, remaining[4]),
        ]
    }

    fn queen_required(&self) -> bool {
        self.move_num > 5 && self.get_remaining()[0] > 0
    }

    fn generate_placements(&self, board_moves: &mut [Option<Move>], n: &mut usize) {
        // First find empty spaces next to the correct color bugs.
        let mut available = HashSet::new();
        for (&loc, tile) in self.grid.iter() {
            if tile.color != self.to_move() {
                continue;
            }
            for &pos in adjacent(loc).iter() {
                if self.get(pos).is_none() {
                    available.insert(pos);
                }
            }
        }

        // Use empty spaces that have no opposite colored tiles adjacent.
        for &pos in available.iter() {
            let placeable = adjacent(pos)
                .iter()
                .all(|adj| self.get(*adj).map(|tile| tile.color == self.to_move()).unwrap_or(true));
            if placeable {
                for (bug, num_left) in self.get_available_bugs().iter() {
                    if self.queen_required() && *bug != Bug::Queen {
                        continue;
                    }
                    if *num_left > 0 {
                        board_moves[*n] = Some(Move::Place(pos, *bug));
                        *n += 1;
                    }
                }
            }
        }
    }
}

#[test]
fn test_gen_placement() {
    let mut board = Board::default();
    for i in 1..5 {
        board.remaining[0][i] = 0;
        board.remaining[1][i] = 0;
    }
    board.insert((0, 0), Bug::Queen, Color::Black);
    board.insert((1, 0), Bug::Queen, Color::White);
    println!("{}", board);
    let mut moves = [None; 100];
    let mut n = 0;
    board.generate_placements(&mut moves, &mut n);
    assert_eq!(3, n);
    moves[..n].sort();
    assert_eq!(moves[0].unwrap().place().unwrap(), (-1, -1));
    assert_eq!(moves[1].unwrap().place().unwrap(), (-1, 0));
    assert_eq!(moves[2].unwrap().place().unwrap(), (0, 1));
}

impl Default for Board {
    fn default() -> Self {
        Board { grid: HashMap::new(), remaining: [[1, 3, 3, 2, 2], [1, 3, 3, 2, 2]], move_num: 0 }
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.fancy_fmt())
    }
}

impl Board {
    fn bounding_box(&self) -> (i8, i8, i8, i8) {
        if self.grid.is_empty() {
            return (0, 1, 0, 1);
        }
        let mut minx = i8::MAX;
        let mut maxx = i8::MIN;
        let mut miny = i8::MAX;
        let mut maxy = i8::MIN;
        for &(x, y) in self.grid.keys() {
            minx = std::cmp::min(minx, x);
            maxx = std::cmp::max(maxx, x);
            miny = std::cmp::min(miny, y);
            maxy = std::cmp::max(maxy, y);
        }
        (minx, maxx - minx + 1, miny, maxy - minx + 1)
    }

    fn fancy_fmt(&self) -> String {
        let mut out = String::new();
        let (startx, dx, starty, dy) = self.bounding_box();
        for y in starty - 1..starty + dy + 1 {
            // Print prefix to get staggered hex rows
            let buflen = dy + starty - y;
            if buflen % 2 == 1 {
                out.push(' ');
            }
            for _ in 0..buflen / 2 {
                out.push('\u{ff0e}');
            }

            for x in startx - 1..startx + dx + 1 {
                if let Some(tile) = self.get((x, y)) {
                    if tile.color == Color::White {
                        // Invert terminal background color for white pieces.
                        out.push_str("\x1b[3m");
                    }
                    out.push(match tile.bug {
                        Bug::Queen => '\u{1f41d}',       // HONEYBEE
                        Bug::Grasshopper => '\u{1f997}', // CRICKET
                        Bug::Spider => '\u{1f577}',      // SPIDER
                        Bug::Ant => '\u{1f41c}',         // ANT
                        Bug::Beetle => '\u{1fab2}',      // BEETLE
                                                          //Bug::Ladybug => '\u{1f41e}'', // LADY BEETLE
                                                          //Bug::Mosquito => '\u{1f99f}', // MOSQUITO
                    });
                    if tile.color == Color::White {
                        // Reset coloring.
                        out.push_str("\x1b[m");
                    }
                } else {
                    // Empty cell. Full width period.
                    out.push('\u{ff0e}');
                }
            }
            out.push('\n');
        }
        out
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Move {
    Place(Loc, Bug),
    Movement(Loc, Loc),
}

// For reproducible tests.
impl Ord for Move {
    fn cmp(&self, other: &Self) -> Ordering {
        match *self {
            Move::Place(loc, bug) => {
                if let Move::Place(loc2, bug2) = other {
                    (loc, bug.index()).cmp(&(*loc2, bug2.index()))
                } else {
                    Ordering::Less
                }
            }
            Move::Movement(start, end) => {
                if let Move::Movement(start2, end2) = other {
                    (start, end).cmp(&(*start2, *end2))
                } else {
                    Ordering::Greater
                }
            }
        }
    }
}

impl PartialOrd for Move {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Move {
    #[cfg(test)]
    fn place(&self) -> Option<Loc> {
        if let Move::Place(loc, _) = self {
            Some(*loc)
        } else {
            None
        }
    }
}

impl minimax::Move for Move {
    type G = Game;
    fn apply(&self, board: &mut Board) {
        match *self {
            Move::Place(loc, bug) => {
                board.insert(loc, bug, board.to_move());
                board.mut_remaining()[bug.index()] -= 1;
            }
            Move::Movement(start, end) => {
                let tile = board.remove(start);
                board.insert(end, tile.bug, tile.color);
            }
        }
        board.move_num += 1;
    }
    fn undo(&self, board: &mut Board) {
        match *self {
            Move::Place(loc, bug) => {
                board.remove(loc);
                board.mut_remaining()[bug.index()] += 1;
            }
            Move::Movement(start, end) => {
                let tile = board.remove(end);
                board.insert(start, tile.bug, tile.color);
            }
        }
        board.move_num -= 1;
    }
}

pub struct Game;

impl minimax::Game for Game {
    type S = Board;
    type M = Move;

    fn generate_moves(
        board: &Board, _: minimax::Player, board_moves: &mut [Option<Move>],
    ) -> usize {
        let mut n = 0;

        // Special case for the first 2 moves:
        if board.move_num < 2 {
            for (bug, _) in board.get_available_bugs().iter() {
                board_moves[n] = Some(Move::Place((board.move_num as i8, 0), *bug));
                n += 1;
            }
            board_moves[n] = None;
            return n;
        }

        // Find placeable positions.
        board.generate_placements(board_moves, &mut n);

        if board.queen_required() {
            // No movement allowed.
            board_moves[n] = None;
            return n;
        }

        // TODO: Identify pieces that are free to move.
        // TODO: cache movability for each tile, and somehow iteratively update it

        // TODO: For moveable pieces, generate all legal moves.

        board_moves[n] = None;
        n
    }

    fn get_winner(_: &Board) -> Option<minimax::Winner> {
        // TODO
        None
    }
}
