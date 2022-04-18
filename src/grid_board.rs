extern crate minimax;
extern crate termcolor;

use std::borrow::Borrow;
use std::cmp::{max, min};
use std::collections::hash_map::DefaultHasher;
//use std::convert::TryInto;
use std::default::Default;
use std::hash::Hasher;
//use std::io::Write;
//use termcolor::WriteColor;

// Board representation: wrapping grid of tile locations.

// Index of a board location.
// TODO: try 32x32 grid and see if perf dies.
pub(crate) type Id = u8;
const ROW_SIZE: Id = 16;
const GRID_SIZE: usize = ROW_SIZE as usize * ROW_SIZE as usize;
// One of the midpoints.
const START_ID: Id = ROW_SIZE / 2 * (ROW_SIZE + 1);

fn adjacent(id: Id) -> [Id; 6] {
    // In clockwise order
    [
        id.wrapping_sub(ROW_SIZE + 1),
        id.wrapping_sub(ROW_SIZE),
        id.wrapping_add(1),
        id.wrapping_add(ROW_SIZE + 1),
        id.wrapping_add(ROW_SIZE),
        id.wrapping_sub(1),
    ]
}

lazy_static! {
    static ref ZOBRIST_TABLE: Box<[u64; GRID_SIZE * 2]> = {
        let mut table = Box::new([0u64; GRID_SIZE * 2]);
        let mut hasher = DefaultHasher::new();
        for i in 0..table.len() {
            hasher.write_usize(i);
            table[i] = hasher.finish();
        }
        table
    };
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Bug {
    Queen = 0,
    Grasshopper = 1,
    Spider = 2,
    Ant = 3,
    Beetle = 4,
    Mosquito = 5,
    Ladybug = 6,
    Pillbug = 7,
}

impl Bug {
    pub fn codepoint(&self) -> char {
        match *self {
            Bug::Queen => '\u{1f41d}',       // HONEYBEE
            Bug::Grasshopper => '\u{1f997}', // CRICKET
            Bug::Spider => '\u{1f577}',      // SPIDER
            Bug::Ant => '\u{1f41c}',         // ANT
            Bug::Beetle => '\u{1fab2}',      // BEETLE
            Bug::Mosquito => '\u{1f99f}',    // MOSQUITO
            Bug::Ladybug => '\u{1f41e}',     // LADY BEETLE
            Bug::Pillbug => '\u{1f48a}',     // PILL, either that or MICROBE
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            Bug::Queen => "queen",
            Bug::Grasshopper => "grasshopper",
            Bug::Spider => "spider",
            Bug::Ant => "ant",
            Bug::Beetle => "beetle",
            Bug::Mosquito => "mosquito",
            Bug::Ladybug => "ladybug",
            Bug::Pillbug => "pillbug",
        }
    }

    pub fn iter_all() -> impl Iterator<Item = Self> {
        [
            Bug::Queen,
            Bug::Grasshopper,
            Bug::Spider,
            Bug::Ant,
            Bug::Beetle,
            Bug::Mosquito,
            Bug::Ladybug,
            Bug::Pillbug,
        ]
        .iter()
        .copied()
    }

    pub fn from_char(c: char) -> Option<Bug> {
        match c.to_ascii_lowercase() {
            'q' => Some(Bug::Queen),
            'g' => Some(Bug::Grasshopper),
            's' => Some(Bug::Spider),
            'a' => Some(Bug::Ant),
            'b' => Some(Bug::Beetle),
            'm' => Some(Bug::Mosquito),
            'l' => Some(Bug::Ladybug),
            'p' => Some(Bug::Pillbug),
            _ => None,
        }
    }

    // Whether this bug can only move (itself) by crawling.
    pub(crate) fn crawler(&self) -> bool {
        matches!(*self, Bug::Ant | Bug::Queen | Bug::Spider | Bug::Pillbug)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Color {
    Black = 1,
    White = 0,
}

// bit 7: Color
// bits 4-6: Bug
// bits 0-3:
//   0 => empty
//   1 => single tile
//   2+ => stacked; index into underworld
// All zeros when node is empty.
#[derive(Clone, Copy)]
pub(crate) struct Node(u8);

impl Node {
    fn new() -> Self {
        Node(0)
    }

    fn new_occupied(bug: Bug, color: Color, under_bits: u8) -> Self {
        Node(((color as u8) << 7) | ((bug as u8) << 4) | under_bits)
    }

    fn color(self) -> Color {
        // Color enum is densely packed in 1 bit.
        unsafe { std::mem::transmute::<u8, Color>(self.0 as u8 >> 7) }
    }

    fn bug(self) -> Bug {
        // Bug enum is densely packed in 3 bits.
        unsafe { std::mem::transmute::<u8, Bug>((self.0 >> 4) & 7) }
    }

    fn occupied(self) -> bool {
        self.0 != 0
    }

    fn under_bits(self) -> u8 {
        self.0 & 0xf
    }
}

#[derive(Clone)]
pub struct Board {
    // Indexed by Id.
    pub(crate) nodes: [Node; GRID_SIZE],
    // Tiles that are under other tiles.
    underworld: [Node; 8],
    remaining: [[u8; 8]; 2],
    queens: [Id; 2],
    pub(crate) move_num: u16,
    zobrist_table: &'static [u64; GRID_SIZE * 2],
    zobrist_hash: u64,
    zobrist_history: Vec<u64>,
    // History of move destinations.
    move_history: Vec<Id>,
}

impl minimax::Zobrist for Board {
    fn zobrist_hash(&self) -> u64 {
        self.zobrist_hash
    }
}

impl Board {
    pub fn to_move(&self) -> Color {
        if self.move_num % 2 == 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    fn zobrist(&self, id: Id, bug: Bug, color: Color, height: u32) -> u64 {
        let hash = self.zobrist_table[(id as usize) << 1 | (color as usize)];
        // I don't really want to multiply the table by another factor of 7*8, so
        // just realign the existing random bits.
        // Also include the color to move hash.
        hash.rotate_left((height << 3) | bug as u32) ^ 0xa6c11b626b105b7c
    }

    fn insert_underworld(&mut self, node: Node) -> u8 {
        for i in 0..self.underworld.len() {
            if !self.underworld[i].occupied() {
                self.underworld[i] = node;
                return i as u8 + 2;
            }
        }
        unreachable!("underworld overflowed");
    }

    fn remove_underworld(&mut self, under_bits: u8) -> Node {
        let node = self.underworld[under_bits as usize - 2];
        self.underworld[under_bits as usize - 2] = Node::new();
        node
    }

    fn insert(&mut self, id: Id, bug: Bug, color: Color) {
        let prev = self.nodes[id as usize];
        let under_bits = if prev.occupied() { self.insert_underworld(prev) } else { 1 };
        self.nodes[id as usize] = Node::new_occupied(bug, color, under_bits);
        self.zobrist_hash ^= self.zobrist(id, bug, color, self.height(id));

        if bug == Bug::Queen {
            self.queens[self.move_num as usize & 1] = id;
        }
    }

    // Asserts that there is something there.
    fn remove(&mut self, id: Id) -> (Bug, Color) {
        let height = self.height(id);
        let prev = self.nodes[id as usize];
        let under_bits = prev.under_bits();
        assert!(under_bits != 0);
        self.nodes[id as usize] =
            if under_bits > 1 { self.remove_underworld(under_bits) } else { Node::new() };
        let bug = prev.bug();
        let color = prev.color();
        self.zobrist_hash ^= self.zobrist(id, bug, color, height);
        if bug == Bug::Queen {
            self.queens[self.move_num as usize & 1] = START_ID;
        }
        (bug, color)
    }

    fn height(&self, id: Id) -> u32 {
        let mut height = 0;
        let mut node = self.nodes[id as usize];
        loop {
            let under_bits = node.under_bits();
            if under_bits < 2 {
                return height + under_bits as u32;
            }
            height += 1;
            node = self.underworld[under_bits as usize - 2];
        }
    }

    fn occupied(&self, id: Id) -> bool {
        self.nodes[id as usize].occupied()
    }

    pub(crate) fn get_remaining(&self) -> &[u8; 8] {
        &self.remaining[self.move_num as usize & 1]
    }

    pub(crate) fn get_opponent_remaining(&self) -> &[u8; 8] {
        &self.remaining[!self.move_num as usize & 1]
    }

    fn mut_remaining(&mut self) -> &mut [u8; 8] {
        &mut self.remaining[self.move_num as usize & 1]
    }

    pub(crate) fn get_available_bugs(&self) -> [(Bug, u8); 8] {
        let remaining = self.get_remaining();
        [
            (Bug::Queen, remaining[Bug::Queen as usize]),
            (Bug::Grasshopper, remaining[Bug::Grasshopper as usize]),
            (Bug::Spider, remaining[Bug::Spider as usize]),
            (Bug::Ant, remaining[Bug::Ant as usize]),
            (Bug::Beetle, remaining[Bug::Beetle as usize]),
            (Bug::Mosquito, remaining[Bug::Mosquito as usize]),
            (Bug::Ladybug, remaining[Bug::Ladybug as usize]),
            (Bug::Pillbug, remaining[Bug::Pillbug as usize]),
        ]
    }

    fn queen_required(&self) -> bool {
        self.move_num > 5 && self.get_remaining()[0] > 0
    }

    pub(crate) fn queens_surrounded(&self) -> [usize; 2] {
        let mut out = [0; 2];
        for (i, entry) in out.iter_mut().enumerate() {
            *entry = adjacent(self.queens[i]).iter().filter(|adj| self.occupied(**adj)).count();
        }
        out
    }

    fn new(remaining: [u8; 8]) -> Self {
        Board {
            nodes: [Node::new(); GRID_SIZE],
            underworld: [Node::new(); 8],
            remaining: [remaining; 2],
            queens: [START_ID; 2],
            move_num: 0,
            zobrist_table: ZOBRIST_TABLE.borrow(),
            zobrist_hash: 0,
            zobrist_history: Vec::new(),
            move_history: Vec::new(),
        }
    }

    pub fn new_core_set() -> Self {
        Self::new([1, 3, 2, 3, 2, 0, 0, 0])
    }

    pub fn new_expansions() -> Self {
        Self::new([1, 3, 2, 3, 2, 1, 1, 1])
    }

    // New board from UHP GameTypeString, e.g. "Base+MLP"
    pub fn new_from_game_type(game_type: &str) -> Option<Self> {
        let mut starting = [1, 3, 2, 3, 2, 0, 0, 0];
        let mut toks = game_type.split('+');
        if toks.next()? != "Base" {
            return None;
        }
        if let Some(exts) = toks.next() {
            for ext in exts.chars() {
                match ext {
                    'M' => starting[Bug::Mosquito as usize] = 1,
                    'L' => starting[Bug::Ladybug as usize] = 1,
                    'P' => starting[Bug::Pillbug as usize] = 1,
                    _ => return None,
                }
            }
        }
        Some(Board::new(starting))
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new_expansions()
    }
}

/*
impl Board {
    fn bounding_box(&self) -> (i8, i8, i8, i8) {
        if self.nodes.iter().all(|node| node.tile.is_none()) {
            return (0, 1, 0, 1);
        }
        let mut minx = i8::MAX;
        let mut maxx = i8::MIN;
        let mut miny = i8::MAX;
        let mut maxy = i8::MIN;
        for (id, loc) in (0..).zip(self.id_to_loc.iter()) {
            if self.get(id).is_some() {
                minx = min(minx, loc.0);
                maxx = max(maxx, loc.0);
                miny = min(miny, loc.1);
                maxy = max(maxy, loc.1);
            }
        }
        (minx, maxx - minx + 1, miny, maxy - miny + 1)
    }

    pub fn fancy_fmt(
        &self, buf: &mut termcolor::Buffer, highlights: &[Loc],
    ) -> std::io::Result<()> {
        let (startx, dx, starty, dy) = self.bounding_box();
        for y in starty - 1..starty + dy + 1 {
            // Print prefix to get staggered hex rows
            let buflen = dy + starty - y;
            if buflen % 2 == 1 {
                buf.write_all(b" ")?;
            }
            for _ in 0..buflen / 2 {
                buf.write_all("\u{ff0e}".as_bytes())?;
            }

            for x in startx - 1..startx + dx + 1 {
                let id = *self.loc_to_id.get(&(x, y)).unwrap_or(&UNASSIGNED);
                if let Some(index) = highlights.iter().position(|&loc| loc == (x, y)) {
                    write!(buf, "{: >2}", index)?;
                    continue;
                }
                if let Some(tile) = self.get(id) {
                    if tile.color == Color::White {
                        // Invert terminal background color for white pieces.
                        buf.set_color(
                            termcolor::ColorSpec::new().set_bg(Some(termcolor::Color::White)),
                        )?;
                    }
                    write!(buf, "{}", tile.bug.codepoint())?;
                    if tile.color == Color::White {
                        // Reset coloring.
                        buf.reset()?;
                    }
                } else {
                    // Empty cell. Full width period.
                    buf.write_all("\u{ff0e}".as_bytes())?;
                }
            }

            // Stagger rows the other way to make the space look rectangular.
            for _ in 0..(y - starty + 1) / 2 {
                buf.write_all("\u{ff0e}".as_bytes())?;
            }

            buf.write_all(b"\n")?;
        }
        Ok(())
    }

    pub(crate) fn println(&self) {
        self.println_highlights(&[]);
    }

    pub(crate) fn println_highlights(&self, highlights: &[Loc]) {
        let writer = termcolor::BufferWriter::stdout(termcolor::ColorChoice::Auto);
        let mut buffer = writer.buffer();
        self.fancy_fmt(&mut buffer, highlights).unwrap();
        writer.print(&buffer).unwrap();
    }
}
     */

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Move {
    Place(Id, Bug),
    Movement(Id, Id),
    Pass,
}

impl Default for Move {
    fn default() -> Move {
        Move::Pass
    }
}

impl minimax::Move for Move {
    type G = Rules;
    fn apply(&self, board: &mut Board) {
        let dest = match *self {
            Move::Place(id, bug) => {
                board.insert(id, bug, board.to_move());
                board.mut_remaining()[bug as usize] -= 1;
                id
            }
            Move::Movement(start, end) => {
                let (bug, color) = board.remove(start);
                board.insert(end, bug, color);
                end
            }
            Move::Pass => 0,
        };
        board.move_num += 1;
        board.zobrist_history.push(board.zobrist_hash);
        // TODO: only put Movements in move history
        board.move_history.push(dest);
    }

    fn undo(&self, board: &mut Board) {
        board.move_num -= 1;
        board.zobrist_history.pop();
        board.move_history.pop();
        match *self {
            Move::Place(id, bug) => {
                board.remove(id);
                board.mut_remaining()[bug as usize] += 1;
            }
            Move::Movement(start, end) => {
                let (bug, color) = board.remove(end);
                board.insert(start, bug, color);
            }
            Move::Pass => {}
        }
    }
}

// Useful utility.
pub(crate) struct NodeSet {
    table: [u32; GRID_SIZE / 32],
}

impl NodeSet {
    fn new() -> NodeSet {
        NodeSet { table: [0; GRID_SIZE / 32] }
    }

    fn set(&mut self, id: Id) {
        self.table[id as usize & 0x7] |= 1 << (id as u32 >> 3);
    }

    pub(crate) fn get(&self, id: Id) -> bool {
        (self.table[id as usize & 0x7] >> (id as u32 >> 3)) & 1 != 0
    }
}

impl Board {
    fn generate_placements(&self, moves: &mut Vec<Move>) {
        // TODO: try smallvec to put these queues on the stack
        let mut queue = vec![self.queens[0]];
        let mut visited = NodeSet::new();
        while let Some(id) = queue.pop() {
            if visited.get(id) {
                continue;
            }
            visited.set(id);
            if self.occupied(id) {
                queue.extend(adjacent(id));
                continue;
            }
            let mut num_buddies = 0;
            let mut num_enemies = 0;
            for adj in adjacent(id) {
                let node = self.nodes[adj as usize];
                if node.occupied() {
                    if node.color() == self.to_move() {
                        num_buddies += 1;
                    } else {
                        num_enemies += 1;
                    }
                }
            }
            if num_buddies > 0 && num_enemies == 0 {
                for (bug, num_left) in self.get_available_bugs().iter() {
                    if self.queen_required() && *bug != Bug::Queen {
                        continue;
                    }
                    if *num_left > 0 {
                        moves.push(Move::Place(id, *bug));
                    }
                }
            }
        }
    }

    // Linear algorithm to find all cut vertexes.
    // Algorithm explanation: https://web.archive.org/web/20180830110222/https://www.eecs.wsu.edu/~holder/courses/CptS223/spr08/slides/graphapps.pdf
    // Example code: https://cp-algorithms.com/graph/cutpoints.html
    //
    // TODO: cache movability for each tile, and somehow iteratively update it
    // Need to persist the DFS tree from an arbitrary root.
    // Adding a tile just adds a leaf to one of its neighbors
    // Removing a tile means recomputing a path to the root for any children of the removed node.
    // Hmm, maybe not. DFS iteration order is important.
    pub(crate) fn find_cut_vertexes(&self) -> NodeSet {
        struct State<'a> {
            board: &'a Board,
            visited: NodeSet,
            immovable: NodeSet,
            // Visitation number in DFS traversal.
            num: [u8; GRID_SIZE],
            // Lowest-numbered node reachable using DFS edges and then at most
            // one back edge.
            low: [u8; GRID_SIZE],
            visit_num: u8,
        }
        let mut state = State {
            board: self,
            visited: NodeSet::new(),
            immovable: NodeSet::new(),
            num: [0; GRID_SIZE],
            low: [0; GRID_SIZE],
            visit_num: 1,
        };
        fn dfs(state: &mut State, id: Id, parent: Id, root: bool) {
            state.visited.set(id);
            state.num[id as usize] = state.visit_num;
            state.low[id as usize] = state.visit_num;
            state.visit_num += 1;
            let mut children = 0;
            for adj in adjacent(id) {
                if !state.board.occupied(adj) {
                    continue;
                }
                if !root && adj == parent {
                    continue;
                }
                if state.visited.get(adj) {
                    state.low[id as usize] = min(state.low[id as usize], state.num[adj as usize]);
                } else {
                    dfs(state, adj, id, false);
                    state.low[id as usize] = min(state.low[id as usize], state.low[adj as usize]);
                    if state.low[adj as usize] >= state.num[id as usize] && !root {
                        state.immovable.set(id);
                    }
                    children += 1;
                }
            }
            if root && children > 1 {
                state.immovable.set(id);
            }
        }

        let start = self.queens[0]; // Some occupied node.
        dfs(&mut state, start, 0, true);
        state.immovable
    }

    // For a position on the outside (whether occupied or not), find all
    // adjacent locations still connected to the hive that are slidable.
    // A slidable position has 2 empty slots next to an occupied slot.
    // For all 2^6 possibilities, there can be 0, 2, or 4 slidable neighbors.
    pub(crate) fn slidable_adjacent<'a>(
        &self, neighbors: &'a mut [Id; 6], origin: Id, id: Id,
    ) -> impl Iterator<Item = Id> + 'a {
        *neighbors = adjacent(id);
        // Each bit is whether neighbor is occupied.
        let mut occupied = 0;
        for neighbor in neighbors.iter().rev() {
            occupied <<= 1;
            // Since the origin bug is moving, we can't crawl around it.
            if self.occupied(*neighbor) && *neighbor != origin {
                occupied |= 1;
            }
        }
        // Wrap around in each direction
        occupied |= occupied << 6 | occupied << 12;
        let slidable = (!occupied & (occupied << 1 ^ occupied >> 1)) >> 6;

        neighbors.iter().enumerate().filter_map(move |(i, &id)| {
            if (slidable >> i) & 1 != 0 {
                Some(id)
            } else {
                None
            }
        })
    }

    // Find all walkable tiles where either the source or the dest is on the hive.
    // Unlike what the original rules say (where a beetle on the hive is
    // unrestricted), climbing bugs need to slide into/out of the higher of
    // source or dest heights.
    // https://www.boardgamegeek.com/thread/332467
    fn slidable_adjacent_beetle<'a>(
        &self, out: &'a mut [Id; 6], orig: Id, id: Id,
    ) -> impl Iterator<Item = Id> + 'a {
        let mut self_height = self.height(id);
        if orig == id {
            self_height -= 1;
        }
        let mut heights = [0; 6];
        let neighbors = adjacent(id);
        for i in 0..6 {
            heights[i] = self.height(neighbors[i]);
        }

        let mut n = 0;
        for i in 0..6 {
            let barrier = max(self_height, heights[i]);
            if barrier == 0 {
                // Walking at height zero uses regular sliding rules.
                continue;
            }
            if heights[(i + 1) % 6] > barrier && heights[(i + 5) % 6] > barrier {
                // Piles on both sides are too high and we cannot pass through.
                continue;
            }
            out[n] = neighbors[i];
            n += 1;
        }

        out.iter().take(n).copied()
    }

    // From any bug on top of a stack.
    fn generate_stack_walking(&self, id: Id, moves: &mut Vec<Move>) {
        let mut buf = [0; 6];
        for adj in self.slidable_adjacent_beetle(&mut buf, id, id) {
            moves.push(Move::Movement(id, adj));
        }
    }

    // Jumping over contiguous linear lines of tiles.
    fn generate_jumps(&self, id: Id, moves: &mut Vec<Move>) {
        for delta in [
            (ROW_SIZE + 1).wrapping_neg(),
            ROW_SIZE.wrapping_neg(),
            1u8.wrapping_neg(),
            1,
            ROW_SIZE,
            ROW_SIZE + 1,
        ] {
            let mut jump = id.wrapping_add(delta);
            let mut dist = 1;
            while self.occupied(jump) {
                jump = jump.wrapping_add(delta);
                dist += 1;
            }
            if dist > 1 {
                moves.push(Move::Movement(id, jump));
            }
        }
    }

    fn generate_walk1(&self, id: Id, moves: &mut Vec<Move>) {
        let mut buf = [0; 6];
        for adj in self.slidable_adjacent(&mut buf, id, id) {
            moves.push(Move::Movement(id, adj));
        }
    }

    fn generate_walk3(&self, orig: Id, moves: &mut Vec<Move>) {
        fn dfs(id: Id, orig: Id, board: &Board, path: &mut Vec<Id>, moves: &mut Vec<Move>) {
            if path.contains(&id) {
                return;
            }
            if path.len() == 3 {
                moves.push(Move::Movement(orig, id));
                return;
            }
            path.push(id);
            let mut buf = [0; 6];
            for adj in board.slidable_adjacent(&mut buf, orig, id) {
                dfs(adj, orig, board, path, moves);
            }
            path.pop();
        }
        let mut path = Vec::with_capacity(3);
        dfs(orig, orig, self, &mut path, moves);
    }

    fn generate_walk_all(&self, orig: Id, moves: &mut Vec<Move>) {
        let mut visited = NodeSet::new();
        let mut queue = vec![orig];
        let mut buf = [0; 6];
        while let Some(node) = queue.pop() {
            if visited.get(node) {
                continue;
            }
            visited.set(node);
            if node != orig {
                moves.push(Move::Movement(orig, node));
            }
            for adj in self.slidable_adjacent(&mut buf, orig, node) {
                queue.push(adj);
            }
        }
    }

    fn generate_ladybug(&self, id: Id, moves: &mut Vec<Move>) {
        let mut buf1 = [0; 6];
        let mut buf2 = [0; 6];
        let mut buf3 = [0; 6];
        let mut step2 = NodeSet::new();
        let mut step3 = NodeSet::new();
        for s1 in self.slidable_adjacent_beetle(&mut buf1, id, id) {
            if self.occupied(s1) {
                for s2 in self.slidable_adjacent_beetle(&mut buf2, id, s1) {
                    if self.occupied(s2) && !step2.get(s2) {
                        step2.set(s2);
                        for s3 in self.slidable_adjacent_beetle(&mut buf3, id, s2) {
                            if !self.occupied(s3) && !step3.get(s3) {
                                step3.set(s3);
                                moves.push(Move::Movement(id, s3));
                            }
                        }
                    }
                }
            }
        }
    }

    fn generate_throws(&self, immovable: &NodeSet, id: Id, moves: &mut Vec<Move>) {
        let mut starts = [0; 6];
        let mut num_starts = 0;
        let mut ends = [0; 6];
        let mut num_ends = 0;
        let mut buf = [0; 6];
        let origin = id.wrapping_add(5); // something not adjacent
        for adj in self.slidable_adjacent_beetle(&mut buf, origin, id) {
            match self.height(adj) {
                0 => {
                    ends[num_ends] = adj;
                    num_ends += 1;
                }
                1 => {
                    if !immovable.get(adj) {
                        starts[num_starts] = adj;
                        num_starts += 1;
                    }
                }
                _ => {}
            }
        }
        for &start in starts[..num_starts].iter() {
            for &end in ends[..num_ends].iter() {
                moves.push(Move::Movement(start, end));
            }
        }
    }

    fn generate_mosquito(&self, id: Id, moves: &mut Vec<Move>) {
        let mut targets = [false; 8];
        for adj in adjacent(id) {
            let node = self.nodes[adj as usize];
            if node.occupied() {
                targets[node.bug() as usize] = true;
            }
        }

        if targets[Bug::Ant as usize] {
            self.generate_walk_all(id, moves);
        } else {
            // Avoid adding strictly duplicative moves to the ant.
            if targets[Bug::Queen as usize]
                || targets[Bug::Beetle as usize]
                || targets[Bug::Pillbug as usize]
            {
                self.generate_walk1(id, moves);
            }
            if targets[Bug::Spider as usize] {
                self.generate_walk3(id, moves);
            }
        }
        if targets[Bug::Grasshopper as usize] {
            self.generate_jumps(id, moves);
        }
        if targets[Bug::Beetle as usize] {
            self.generate_stack_walking(id, moves);
        }
        if targets[Bug::Ladybug as usize] {
            self.generate_ladybug(id, moves);
        }
    }

    fn generate_movements(&self, moves: &mut Vec<Move>) {
        let mut immovable = self.find_cut_vertexes();
        let stunned = self.move_history.last();
        if let Some(moved) = stunned {
            // Can't move pieces that were moved on the opponent's turn.
            immovable.set(*moved);
        }

        let mut dedup = false;
        let mut queue = vec![self.queens[0]];
        let mut visited = NodeSet::new();
        while let Some(id) = queue.pop() {
            if visited.get(id) {
                continue;
            }
            visited.set(id);
            let node = self.nodes[id as usize];
            if !node.occupied() {
                continue;
            }
            if node.color() != self.to_move() {
                continue;
            }
            if node.under_bits() > 1 {
                self.generate_stack_walking(id, moves);
                // Don't let mosquito on stack use pillbug ability.
                // Although the rules don't seem to specify either way.
                continue;
            }
            // Check for throw ability before movability, as pinned pillbugs can still throw.
            let pillbug_powers = node.bug() == Bug::Pillbug
                || (node.bug() == Bug::Mosquito
                    && adjacent(id).iter().any(|&adj| {
                        let n = self.nodes[adj as usize];
                        n.occupied() && n.bug() == Bug::Pillbug
                    }));
            // However pillbugs just thrown cannot throw.
            if pillbug_powers && stunned != Some(&id) {
                self.generate_throws(&immovable, id, moves);
                dedup = true;
            }
            if immovable.get(id) {
                continue;
            }
            match node.bug() {
                Bug::Queen => self.generate_walk1(id, moves),
                Bug::Grasshopper => self.generate_jumps(id, moves),
                Bug::Spider => self.generate_walk3(id, moves),
                Bug::Ant => self.generate_walk_all(id, moves),
                Bug::Beetle => {
                    self.generate_walk1(id, moves);
                    self.generate_stack_walking(id, moves);
                }
                Bug::Mosquito => {
                    self.generate_mosquito(id, moves);
                    dedup = true;
                }
                Bug::Ladybug => self.generate_ladybug(id, moves),
                Bug::Pillbug => self.generate_walk1(id, moves),
            }
        }

        if dedup {
            // Mosquitos and pillbugs can create duplicate moves, so sort and dedup.
            moves.sort_unstable();
            moves.dedup();
        }
    }
}

pub struct Rules;

impl minimax::Game for Rules {
    type S = Board;
    type M = Move;

    fn generate_moves(board: &Board, moves: &mut Vec<Move>) {
        if board.move_num < 2 {
            // Special case for the first 2 moves:
            for (bug, num_left) in board.get_available_bugs().iter() {
                if *bug == Bug::Queen {
                    // To reduce draws, implement tournament rule where
                    // you can't place your queen first.
                    continue;
                }
                if *num_left > 0 {
                    if board.move_num == 0 {
                        moves.push(Move::Place(START_ID, *bug));
                    } else {
                        for &id in adjacent(START_ID).iter() {
                            moves.push(Move::Place(id, *bug));
                        }
                    }
                }
            }
        } else {
            // Once queen has been placed, pieces may move.
            if board.get_remaining()[Bug::Queen as usize] == 0 {
                // For movable pieces, generate all legal moves.
                board.generate_movements(moves);
            }

            // Find placeable positions.
            board.generate_placements(moves);
        }

        if moves.is_empty() {
            moves.push(Move::Pass);
        }
    }

    fn get_winner(board: &Board) -> Option<minimax::Winner> {
        let queens_surrounded = board.queens_surrounded();
        let n = board.move_num as usize;
        if n > 10 {
            // Check for position repeat stalemate.
            // More than 32 moves ago, we're not going to bother looking.
            // 4 is the most recently this move could have occurred.
            let start = if n < 32 { !n & 1 } else { n - 32 };
            let recent_past = &board.zobrist_history[start..n - 4];
            let position_repeat_count =
                recent_past.iter().step_by(2).filter(|&&hash| hash == board.zobrist_hash).count();
            if position_repeat_count == 2 {
                // Draw by stalemate.
                return Some(minimax::Winner::Draw);
            }
        }

        if queens_surrounded == [6, 6] {
            // Draw by simultaneous queen surrounding.
            Some(minimax::Winner::Draw)
        } else if queens_surrounded[board.to_move() as usize] == 6 {
            Some(minimax::Winner::PlayerJustMoved)
        } else if queens_surrounded[1 - board.to_move() as usize] == 6 {
            Some(minimax::Winner::PlayerToMove)
        } else {
            None
        }
    }
}

type Loc = (i8, i8);
fn loc_to_id(loc: Loc) -> Id {
    // Centered in the middle of the board.
    START_ID.wrapping_add(ROW_SIZE.wrapping_mul(loc.1 as u8)).wrapping_add(loc.0 as u8)
}

fn id_to_loc(id: Id) -> Loc {
    let mut x = (id.wrapping_sub(START_ID - ROW_SIZE / 2) / ROW_SIZE) as i8;
    if x > 7 {
        x -= ROW_SIZE as i8;
    }
    let mut y = (id.wrapping_sub(START_ID) % ROW_SIZE) as i8;
    if y > 7 {
        y -= ROW_SIZE as i8;
    }
    (y, x)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_id_loc() {
        for x in -3..4 {
            for y in -3..4 {
                let loc = (x, y);
                let id = loc_to_id(loc);
                assert_eq!(loc, id_to_loc(id), "{}", id);
            }
        }
    }

    impl Board {
        fn insert_loc(&mut self, loc: Loc, bug: Bug, color: Color) {
            self.insert(loc_to_id(loc), bug, color);
        }

        fn remove_loc(&mut self, loc: Loc) {
            self.remove(loc_to_id(loc));
        }

        fn fill_board(&mut self, locs: &[Loc], bug: Bug) {
            for &loc in locs {
                self.insert(loc_to_id(loc), bug, Color::Black);
            }
        }

        fn assert_placements(&self, moves: &[Move], expected: &[(Loc, Bug)]) {
            let mut actual_pairs = Vec::new();
            for &m in moves.iter() {
                if let Move::Place(actual_id, actual_bug) = m {
                    actual_pairs.push((id_to_loc(actual_id), actual_bug));
                }
            }
            actual_pairs.sort();
            let mut expected_pairs = Vec::new();
            expected_pairs.extend(expected);
            expected_pairs.sort();
            assert_eq!(actual_pairs, expected_pairs);
        }

        fn assert_movements(&self, moves: &[Move], start: Loc, ends: &[Loc]) {
            let mut actual_ends = Vec::new();
            for &m in moves.iter() {
                if let Move::Movement(actual_start, actual_end) = m {
                    if id_to_loc(actual_start) == start {
                        actual_ends.push(id_to_loc(actual_end));
                    }
                }
            }
            actual_ends.sort();
            let mut expected_ends = Vec::new();
            expected_ends.extend(ends);
            expected_ends.sort();
            assert_eq!(actual_ends, expected_ends);
        }
    }

    #[test]
    fn test_gen_placement() {
        let mut board = Board::default();
        for i in 1..8 {
            board.remaining[0][i] = 0;
            board.remaining[1][i] = 0;
        }
        board.insert(loc_to_id((0, 0)), Bug::Queen, Color::White);
        board.insert(loc_to_id((1, 0)), Bug::Queen, Color::Black);
        let mut moves = Vec::new();
        board.generate_placements(&mut moves);
        board.assert_placements(
            &moves,
            &[((-1, -1), Bug::Queen), ((-1, 0), Bug::Queen), ((0, 1), Bug::Queen)],
        );
    }

    #[test]
    fn test_cut_vertex() {
        let mut board = Board::default();
        //．．🐝🐝🐝🐝
        // ．．．🐝．🐝🐝
        //．．．．🐝🐝
        board.fill_board(
            &[(0, 0), (0, 1), (1, 0), (2, 1), (1, 2), (2, 2), (-1, 0), (-2, 0), (3, 1)],
            Bug::Queen,
        );
        let cuts = board.find_cut_vertexes();
        let mut cut_locs = vec![];
        for id in 0..255 {
            if cuts.get(id) {
                cut_locs.push(id_to_loc(id));
            }
        }
        assert_eq!(&[(-1, 0), (2, 1)], &cut_locs[..]);
    }

    #[test]
    fn test_slidable() {
        let mut board = Board::default();
        let x = START_ID;
        let mut buf = [0; 6];
        // One neighbor.
        board.insert_loc((0, 0), Bug::Queen, Color::Black);
        board.insert_loc((1, 0), Bug::Queen, Color::Black);
        assert_eq!(
            vec![loc_to_id((0, -1)), loc_to_id((1, 1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Id>>()
        );
        // Two adjacent neighbors.
        board.insert_loc((1, 1), Bug::Queen, Color::Black);
        assert_eq!(
            vec![loc_to_id((0, -1)), loc_to_id((0, 1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Id>>()
        );
        // Four adjacent neighbors.
        board.insert_loc((0, 1), Bug::Queen, Color::Black);
        board.insert_loc((-1, 0), Bug::Queen, Color::Black);
        assert_eq!(
            vec![loc_to_id((-1, -1)), loc_to_id((0, -1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Id>>()
        );
        // Five adjacent neighbors.
        board.insert_loc((-1, -1), Bug::Queen, Color::Black);
        assert_eq!(Vec::<Id>::new(), board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Id>>());
        // 2 separated groups of neighbors.
        board.remove_loc((0, 1));
        assert_eq!(Vec::<Id>::new(), board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Id>>());
        // 2 opposite single neighbors
        board.remove_loc((1, 1));
        board.remove_loc((-1, -1));
        assert_eq!(
            vec![loc_to_id((-1, -1)), loc_to_id((0, -1)), loc_to_id((1, 1)), loc_to_id((0, 1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Id>>()
        );
    }

    #[test]
    fn test_generate_jumps() {
        let mut board = Board::default();
        //．．．🦗🦗🦗．
        // ．．🦗．．．
        //．．．．．．
        // ．🦗．．
        board.fill_board(&[(0, 0), (0, 1), (0, 3), (1, 0), (2, 0)], Bug::Grasshopper);
        let mut moves = Vec::new();
        board.generate_jumps(START_ID, &mut moves);
        board.assert_movements(&moves, (0, 0), &[(0, 2), (3, 0)]);
    }

    #[test]
    fn test_generate_beetle() {
        let mut board = Board::default();
        board.fill_board(
            &[
                (0, 0),
                (0, 0),
                (-1, -1),
                (-1, -1),
                (0, 1),
                (0, 1),
                (0, -1),
                (0, -1),
                (0, -1),
                (1, 0),
                (1, 0),
                (1, 1),
                (1, 1),
                (1, 1),
            ],
            Bug::Beetle,
        );
        // Stack heights:
        //   2   3
        //  0 (2) 2
        //   2   3
        // Can't move left (down) or right (up) because of blocking stacks.
        // Can move onto all 4 blocking stacks.
        let mut moves = Vec::new();
        board.generate_stack_walking(START_ID, &mut moves);
        board.assert_movements(&moves, (0, 0), &[(-1, -1), (0, -1), (0, 1), (1, 1)]);
    }

    #[test]
    fn test_generate_walk3() {
        let mut board = Board::default();
        //．．．🕷．．．．．
        // ．．．🕷．🕷．．
        //．．．🕷．．🕷．
        // ．．．🕷🕷🕷
        board.fill_board(
            &[(-1, -1), (0, 0), (2, 0), (0, 1), (3, 1), (1, 2), (2, 2), (3, 2)],
            Bug::Spider,
        );
        let mut moves = Vec::new();
        let start = loc_to_id((-1, -1));
        board.generate_walk3(start, &mut moves);
        board.assert_movements(&moves, (-1, -1), &[(0, 2), (1, -1), (1, 1), (2, 1)]);

        // ．．🕷．🕷．．
        //．．🕷🕷．🕷．
        // ．．🕷🕷🕷
        board.remove_loc((-1, -1));
        board.insert_loc((1, 1), Bug::Spider, Color::Black);
        moves.clear();
        let start = loc_to_id((1, 1));
        board.generate_walk3(start, &mut moves);
        board.assert_movements(&moves, (1, 1), &[(-1, -1), (0, -1), (1, -1), (2, -1)]);
    }

    #[test]
    fn test_generate_walk_all() {
        let mut board = Board::default();
        //．．．🐜．．．．
        // ．．．🐜．．．
        //．．．🐜．🐜．
        // ．．．🐜🐜
        board.fill_board(&[(-1, -1), (0, 0), (0, 1), (2, 1), (1, 2), (2, 2)], Bug::Ant);
        let mut moves = Vec::new();
        let start = loc_to_id((-1, -1));
        board.generate_walk_all(start, &mut moves);
        board.assert_movements(
            &moves,
            (-1, -1),
            &[
                (0, -1),
                (-1, 0),
                (1, 0),
                (2, 0),
                (-1, 1),
                (3, 1),
                (0, 2),
                (3, 2),
                (1, 3),
                (2, 3),
                (3, 3),
            ],
        );
    }

    #[test]
    fn test_generate_mosquito() {
        let mut board = Board::default();
        board.fill_board(&[(0, 0), (1, 1)], Bug::Mosquito);
        let mut moves = Vec::new();
        board.generate_mosquito(loc_to_id((0, 0)), &mut moves);
        // Mosquito on mosquito can't move at all.
        board.assert_movements(&moves, (0, 0), &[]);

        //．．🦟🦗．
        // ．🐜🪲．
        board.insert_loc((0, 1), Bug::Ant, Color::Black);
        board.insert_loc((1, 1), Bug::Beetle, Color::Black);
        board.insert_loc((1, 0), Bug::Grasshopper, Color::Black);
        moves.clear();
        // Dedup happens in generate_movements.
        board.move_num += 1;
        board.generate_movements(&mut moves);
        board.assert_movements(
            &moves,
            (0, 0),
            &[
                (-1, 0),
                (-1, 1),
                (0, -1),
                (0, 1),
                (0, 2),
                (1, -1),
                (1, 0),
                (1, 1),
                (1, 2),
                (2, 0),
                (2, 1),
                (2, 2),
            ],
        );
    }

    #[test]
    fn test_generate_ladybug() {
        let mut board = Board::default();
        board.fill_board(&[(2, 3), (0, 0), (0, 1), (2, 1), (1, 2), (2, 2)], Bug::Ladybug);
        //．．．🐞．．．
        // ．．🐞．🐞．．
        //．．．🐞🐞．．
        // ．．．🐞．．
        let mut moves = Vec::new();
        let start = loc_to_id((2, 3));
        board.generate_ladybug(start, &mut moves);
        board.assert_movements(
            &moves,
            (2, 3),
            &[(-1, 0), (1, 0), (2, 0), (-1, 1), (1, 1), (3, 1), (0, 2), (3, 2), (1, 3), (3, 3)],
        );
    }

    #[test]
    fn test_generate_throws() {
        let mut board = Board::default();
        board.fill_board(&[(1, 1), (0, 0), (0, 0), (2, 2), (2, 2), (0, 1), (1, 2)], Bug::Pillbug);
        // ．．💊．．．
        //．．💊💊．．
        // ．．💊💊．
        let mut moves = Vec::new();
        let immovable = NodeSet::new();
        let start = loc_to_id((1, 1));
        board.generate_throws(&immovable, start, &mut moves);
        assert_eq!(4, moves.len());
        board.assert_movements(&moves[..2], (1, 2), &[(1, 0), (2, 1)]);
        board.assert_movements(&moves[2..], (0, 1), &[(1, 0), (2, 1)]);

        // Create a level-2 gate to prevent one piece from being thrown.
        board.remove_loc((0, 0));
        board.insert_loc((0, 1), Bug::Pillbug, Color::Black);
        moves.clear();
        board.generate_throws(&immovable, start, &mut moves);
        assert_eq!(2, moves.len());
        board.assert_movements(&moves, (0, 0), &[(1, 0), (2, 1)]);

        // Create a level-2 gate to prevent one destination to being thrown to.
        board.insert_loc((1, 0), Bug::Pillbug, Color::Black);
        board.insert_loc((1, 0), Bug::Pillbug, Color::Black);
        board.remove_loc((0, 1));
        board.remove_loc((0, 1));
        board.remove_loc((1, 2));
        moves = Vec::new();
        board.generate_throws(&immovable, start, &mut moves);
        assert_eq!(2, moves.len());
        board.assert_movements(&moves, (0, 0), &[(0, 1), (1, 2)]);
    }

    #[test]
    fn test_winner() {
        use minimax::{Game, Move};

        // Draw by stalemate
        let mut board = Board::default();
        let x1 = loc_to_id((-1, -1));
        let x2 = loc_to_id((-1, 0));
        let y1 = loc_to_id((1, 1));
        let y2 = loc_to_id((1, 0));
        super::Move::Place(loc_to_id((0, 0)), Bug::Spider).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        super::Move::Place(x1, Bug::Queen).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        // Create the position the first time.
        super::Move::Place(y1, Bug::Queen).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        super::Move::Movement(x1, x2).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        super::Move::Movement(y1, y2).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        super::Move::Movement(x2, x1).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        // Recreate position for the second time.
        super::Move::Movement(y2, y1).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        super::Move::Movement(x1, x2).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        super::Move::Movement(y1, y2).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        super::Move::Movement(x2, x1).apply(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
        // Recreate position for the third time.
        super::Move::Movement(y2, y1).apply(&mut board);
        assert_eq!(Some(minimax::Winner::Draw), Rules::get_winner(&board));
        // Undo reverts zobrist and history.
        super::Move::Movement(y2, y1).undo(&mut board);
        assert_eq!(None, Rules::get_winner(&board));
    }
}
