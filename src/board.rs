extern crate minimax;

use crate::bug::Bug;
use crate::hex_grid::*;
use std::borrow::Borrow;
use std::cmp::{max, min};
use std::collections::hash_map::DefaultHasher;
use std::default::Default;
use std::hash::Hasher;

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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Color {
    Black = 1,
    White = 0,
}

impl Color {
    pub fn other(self) -> usize {
        1 - self as usize
    }
}

// bit 7: Color
// bits 4-6: Bug
// bits 3-4: Bug num
// bits 0-1: tile height; capped at 3
//   see underworld for covered tiles.
// All zeros when node is empty.
#[derive(Clone, Copy)]
pub struct Node(u8);

impl Node {
    fn empty() -> Self {
        Node(0)
    }

    fn new_occupied(bug: Bug, color: Color, bug_num: u8, clipped_height: u8) -> Self {
        Node(((color as u8) << 7) | ((bug as u8) << 4) | (bug_num << 2) | clipped_height)
    }

    fn with_height(self, height: u8) -> Self {
        let clipped_height = min(height, 3);
        Node((self.0 & !3) | clipped_height)
    }

    pub(crate) fn color(self) -> Color {
        // Color enum is densely packed in 1 bit.
        unsafe { std::mem::transmute::<u8, Color>(self.0 >> 7) }
    }

    pub(crate) fn bug(self) -> Bug {
        // Bug enum is densely packed in 3 bits.
        unsafe { std::mem::transmute::<u8, Bug>((self.0 >> 4) & 7) }
    }

    pub(crate) fn bug_num(self) -> u8 {
        (self.0 >> 2) & 3
    }

    pub(crate) fn occupied(self) -> bool {
        self.0 != 0
    }

    pub(crate) fn is_stacked(self) -> bool {
        self.clipped_height() > 1
    }

    pub(crate) fn clipped_height(self) -> u8 {
        self.0 & 0x3
    }
}

#[derive(Copy, Clone)]
pub struct UnderNode {
    // What bug is here.
    node: Node,
    // Where is this stack.
    hex: Hex,
    // What is the height of this piece.
    height: u8,
}

impl UnderNode {
    fn new(node: Node, hex: Hex, height: u8) -> Self {
        Self { node: node.with_height(height), hex, height }
    }

    fn empty() -> Self {
        Self { node: Node::empty(), hex: 0, height: 0 }
    }

    pub fn node(&self) -> Node {
        self.node
    }
    pub fn hex(&self) -> Hex {
        self.hex
    }
}

#[derive(Clone)]
pub struct Board {
    // Indexed by Hex.
    pub(crate) nodes: [Node; GRID_SIZE],
    // Tiles that are under other tiles.
    // Sorted by height (for each hex) and no gaps.
    underworld: [UnderNode; 8],
    underworld_size: usize,
    pub(crate) remaining: [[u8; 8]; 2],
    pub(crate) queens: [Hex; 2],
    pub(crate) occupied_hexes: [Vec<Hex>; 2],

    pub(crate) turn_num: u16,
    zobrist_table: &'static [u64; GRID_SIZE * 2],
    zobrist_hash: u64,
    zobrist_history: Vec<u64>,
    // Board history.
    pub(super) turn_history: Vec<Turn>,

    pub(super) game_type_bits: u8,
}

impl Board {
    pub fn to_move(&self) -> Color {
        if self.turn_num % 2 == 0 {
            Color::White
        } else {
            Color::Black
        }
    }

    pub(crate) fn node(&self, hex: Hex) -> Node {
        self.nodes[hex as usize]
    }

    fn zobrist(&self, hex: Hex, bug: Bug, color: Color, height: u8) -> u64 {
        let hash = self.zobrist_table[(hex as usize) << 1 | (color as usize)];
        // I don't really want to multiply the table by another factor of 7*8, so
        // just realign the existing random bits.
        // Also include the color to move hash.
        hash.rotate_left(((height as u32) << 3) | bug as u32)
    }

    pub fn get_underworld(&self) -> &[UnderNode] {
        &self.underworld[0..self.underworld_size]
    }

    fn insert_underworld(&mut self, node: Node, hex: Hex) {
        let height = self.underworld_height(hex, node);
        if self.underworld_size >= self.underworld.len() {
            unreachable!("underworld overflowed");
        }
        self.underworld[self.underworld_size] = UnderNode::new(node, hex, height);
        self.underworld_size += 1;
    }

    fn remove_underworld(&mut self, hex: Hex) -> Node {
        for i in (0..self.underworld_size).rev() {
            if self.underworld[i].hex == hex {
                let node = self.underworld[i].node;
                self.underworld[i..self.underworld_size].rotate_left(1);
                self.underworld_size -= 1;
                return node;
            }
        }
        unreachable!("underworld not found");
    }

    fn occupied_add(&mut self, color: Color, hex: Hex) {
        self.occupied_hexes[color as usize].push(hex);
    }

    fn occupied_remove(&mut self, color: Color, hex: Hex) {
        let vec = &mut self.occupied_hexes[color as usize];
        let i = vec.iter().position(|&x| x == hex).unwrap();
        vec.swap_remove(i);
    }

    fn insert(&mut self, hex: Hex, bug: Bug, bug_num: u8, color: Color) {
        let prev = self.node(hex);
        if prev.occupied() {
            if prev.color() != color {
                self.occupied_remove(prev.color(), hex);
                self.occupied_add(color, hex);
            }
        } else {
            self.occupied_add(color, hex);
        }
        if prev.occupied() {
            self.insert_underworld(prev, hex);
        }
        let clipped_height = min(prev.clipped_height() + 1, 3);
        self.nodes[hex as usize] = Node::new_occupied(bug, color, bug_num, clipped_height);
        self.zobrist_hash ^= self.zobrist(hex, bug, color, self.height(hex));

        if bug == Bug::Queen {
            self.queens[color as usize] = hex;
        }
    }

    // Asserts that there is something there.
    fn remove(&mut self, hex: Hex) -> (Bug, u8, Color) {
        let height = self.height(hex);
        let prev = self.node(hex);

        let new_node = if height > 1 { self.remove_underworld(hex) } else { Node::empty() };
        self.nodes[hex as usize] = new_node;
        let bug = prev.bug();
        let color = prev.color();
        if new_node.occupied() {
            if new_node.color() != color {
                self.occupied_remove(color, hex);
                self.occupied_add(new_node.color(), hex);
            }
        } else {
            self.occupied_remove(color, hex);
        }

        self.zobrist_hash ^= self.zobrist(hex, bug, color, height);
        if bug == Bug::Queen {
            self.queens[color as usize] = START_HEX;
        }
        (bug, prev.bug_num(), color)
    }

    fn underworld_height(&self, hex: Hex, node: Node) -> u8 {
        let height = node.clipped_height();
        if height > 2 {
            1 + self.underworld[0..self.underworld_size]
                .iter()
                .rev()
                .find(|under| under.hex == hex)
                .unwrap()
                .height
        } else {
            height
        }
    }

    fn height(&self, hex: Hex) -> u8 {
        self.underworld_height(hex, self.node(hex))
    }

    pub(crate) fn occupied(&self, hex: Hex) -> bool {
        self.node(hex).occupied()
    }

    pub(crate) fn get_remaining(&self) -> &[u8; 8] {
        &self.remaining[self.turn_num as usize & 1]
    }

    pub(crate) fn get_opponent_remaining(&self) -> &[u8; 8] {
        &self.remaining[!self.turn_num as usize & 1]
    }

    fn mut_remaining(&mut self) -> &mut [u8; 8] {
        &mut self.remaining[self.turn_num as usize & 1]
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

    pub(crate) fn queen_required(&self) -> bool {
        self.turn_num > 5 && self.get_remaining()[Bug::Queen as usize] > 0
    }

    pub(crate) fn queens_surrounded(&self) -> [usize; 2] {
        let mut out = [0; 2];
        for (i, entry) in out.iter_mut().enumerate() {
            *entry = adjacent(self.queens[i]).iter().filter(|adj| self.occupied(**adj)).count();
        }
        out
    }

    pub(super) fn new(remaining: [u8; 8]) -> Self {
        let mut game_type_bits = 0u8;
        for (i, &remain) in remaining.iter().enumerate() {
            if remain > 0 {
                game_type_bits |= 1 << i;
            }
        }
        Board {
            nodes: [Node::empty(); GRID_SIZE],
            underworld: [UnderNode::empty(); 8],
            underworld_size: 0,
            remaining: [remaining; 2],
            queens: [START_HEX; 2],
            occupied_hexes: [Vec::new(), Vec::new()],
            turn_num: 0,
            zobrist_table: ZOBRIST_TABLE.borrow(),
            zobrist_hash: 0,
            zobrist_history: Vec::new(),
            turn_history: Vec::new(),
            game_type_bits,
        }
    }

    pub fn new_core_set() -> Self {
        Self::new([1, 3, 2, 3, 2, 0, 0, 0])
    }

    pub fn new_expansions() -> Self {
        Self::new([1, 3, 2, 3, 2, 1, 1, 1])
    }
}

impl Default for Board {
    fn default() -> Self {
        Self::new_expansions()
    }
}

#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Ord, PartialOrd)]
pub enum Turn {
    Place(Hex, Bug),
    Move(Hex, Hex),
    #[default]
    Pass,
}

impl Board {
    pub fn apply(&mut self, turn: Turn) {
        match turn {
            Turn::Place(hex, bug) => {
                let bug_num =
                    Bug::initial_quantity()[bug as usize] - self.get_remaining()[bug as usize] + 1;
                self.insert(hex, bug, bug_num, self.to_move());
                self.mut_remaining()[bug as usize] -= 1;
            }
            Turn::Move(start, end) => {
                let (bug, bug_num, color) = self.remove(start);
                self.insert(end, bug, bug_num, color);
                // Encode a marker of last moved location for pillbug throwability.
                self.zobrist_hash ^= end as u64;
            }
            _ => {}
        };
        self.turn_num += 1;
        // Encode positions differently based on who is to move.
        self.zobrist_hash ^= 0xa6c11b626b105b7c;
        // Undo last-moved zobrist bits.
        if let Some(Turn::Move(_, end)) = self.turn_history.last() {
            self.zobrist_hash ^= *end as u64;
        }
        self.zobrist_history.push(self.zobrist_hash);
        self.turn_history.push(turn);
    }

    pub fn undo(&mut self, turn: Turn) {
        self.turn_num -= 1;
        self.zobrist_history.pop();
        self.turn_history.pop();
        if let Some(Turn::Move(_, end)) = self.turn_history.last() {
            self.zobrist_hash ^= *end as u64;
        }
        match turn {
            Turn::Place(hex, bug) => {
                self.remove(hex);
                self.mut_remaining()[bug as usize] += 1;
            }
            Turn::Move(start, end) => {
                let (bug, bug_num, color) = self.remove(end);
                self.insert(start, bug, bug_num, color);
                self.zobrist_hash ^= end as u64;
            }
            Turn::Pass => {}
        }
        // Encode positions differently based on who is to move.
        self.zobrist_hash ^= 0xa6c11b626b105b7c;
    }
}

impl Board {
    fn generate_placements(&self, turns: &mut Vec<Turn>) {
        let mut enemy_adjacent = HexSet::new();
        for &enemy in self.occupied_hexes[self.to_move().other()].iter() {
            for adj in adjacent(enemy) {
                enemy_adjacent.set(adj);
            }
        }

        let mut visited = HexSet::new();
        for &friend in self.occupied_hexes[self.to_move() as usize].iter() {
            for hex in adjacent(friend) {
                if visited.get(hex) {
                    continue;
                }
                visited.set(hex);
                if self.occupied(hex) || enemy_adjacent.get(hex) {
                    continue;
                }
                for (bug, num_left) in self.get_available_bugs().iter() {
                    if self.queen_required() && *bug != Bug::Queen {
                        continue;
                    }
                    if *num_left > 0 {
                        turns.push(Turn::Place(hex, *bug));
                    }
                }
            }
        }
    }

    // Linear algorithm to find all cut vertexes.
    // Algorithm explanation: https://web.archive.org/web/20180830110222/https://www.eecs.wsu.edu/~holder/courses/CptS223/spr08/slides/graphapps.pdf
    // Example code: https://cp-algorithms.com/graph/cutpoints.html
    pub(crate) fn find_cut_vertexes(&self) -> HexSet {
        struct State<'a> {
            board: &'a Board,
            visited: HexSet,
            immovable: HexSet,
            // Visitation number in DFS traversal.
            num: [u8; GRID_SIZE],
            // Lowest-numbered node reachable using DFS edges and then at most
            // one back edge.
            low: [u8; GRID_SIZE],
            visit_num: u8,
        }
        let mut state = State {
            board: self,
            visited: HexSet::new(),
            immovable: HexSet::new(),
            num: [0; GRID_SIZE],
            low: [0; GRID_SIZE],
            visit_num: 1,
        };
        fn dfs(state: &mut State, hex: Hex, parent: Hex) {
            state.visited.set(hex);
            state.num[hex as usize] = state.visit_num;
            state.low[hex as usize] = state.visit_num;
            state.visit_num += 1;
            let root = hex == parent;
            let mut children = 0;
            for adj in adjacent(hex) {
                if !state.board.occupied(adj) {
                    continue;
                }
                if adj == parent {
                    continue;
                }
                if state.visited.get(adj) {
                    state.low[hex as usize] = min(state.low[hex as usize], state.num[adj as usize]);
                } else {
                    dfs(state, adj, hex);
                    state.low[hex as usize] = min(state.low[hex as usize], state.low[adj as usize]);
                    if state.low[adj as usize] >= state.num[hex as usize] && !root {
                        state.immovable.set(hex);
                    }
                    children += 1;
                }
            }
            if root && children > 1 {
                state.immovable.set(hex);
            }
        }

        let start = self.queens[0]; // Some occupied node.
        dfs(&mut state, start, start);
        state.immovable
    }

    // For a position on the outside (whether occupied or not), find all
    // adjacent locations still connected to the hive that are slidable.
    // A slidable position has 2 empty slots next to an occupied slot.
    // For all 2^6 possibilities, there can be 0, 2, or 4 slidable neighbors.
    pub(crate) fn slidable_adjacent<'a>(
        &self, neighbors: &'a mut [Hex; 6], origin: Hex, hex: Hex,
    ) -> impl Iterator<Item = Hex> + 'a {
        *neighbors = adjacent(hex);
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

        neighbors.iter().enumerate().filter_map(move |(i, &hex)| {
            if (slidable >> i) & 1 != 0 {
                Some(hex)
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
        &self, out: &'a mut [Hex; 6], orig: Hex, hex: Hex,
    ) -> impl Iterator<Item = Hex> + 'a {
        let mut self_height = self.height(hex);
        if orig == hex {
            self_height -= 1;
        }
        let mut heights = [0; 6];
        let neighbors = adjacent(hex);
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
    fn generate_stack_walking(&self, hex: Hex, turns: &mut Vec<Turn>) {
        let mut buf = [0; 6];
        for adj in self.slidable_adjacent_beetle(&mut buf, hex, hex) {
            turns.push(Turn::Move(hex, adj));
        }
    }

    // Jumping over contiguous linear lines of tiles.
    fn generate_jumps(&self, hex: Hex, turns: &mut Vec<Turn>) {
        for dir in Direction::all() {
            let mut jump = dir.apply(hex);
            let mut dist = 1;
            while self.occupied(jump) {
                jump = dir.apply(jump);
                dist += 1;
                if jump == hex {
                    // Exit out if we'd infinitey loop.
                    dist = 0;
                    break;
                }
            }
            if dist > 1 {
                turns.push(Turn::Move(hex, jump));
            }
        }
    }

    fn generate_walk1(&self, hex: Hex, turns: &mut Vec<Turn>) {
        let mut buf = [0; 6];
        for adj in self.slidable_adjacent(&mut buf, hex, hex) {
            turns.push(Turn::Move(hex, adj));
        }
    }

    fn generate_walk3(&self, orig: Hex, turns: &mut Vec<Turn>) {
        let mut buf1 = [0; 6];
        let mut buf2 = [0; 6];
        let mut buf3 = [0; 6];
        let mut visited = HexSet::new();
        visited.set(orig);

        for s1 in self.slidable_adjacent(&mut buf1, orig, orig) {
            for s2 in self.slidable_adjacent(&mut buf2, orig, s1) {
                if s2 != orig {
                    for s3 in self.slidable_adjacent(&mut buf3, orig, s2) {
                        if s3 != s1 && !visited.get(s3) {
                            turns.push(Turn::Move(orig, s3));
                            visited.set(s3);
                        }
                    }
                }
            }
        }
    }

    fn generate_walk_all(&self, orig: Hex, turns: &mut Vec<Turn>) {
        let mut visited = HexSet::new();
        let mut queue = [0; 16];
        queue[0] = orig;
        let mut qsize = 1;
        let mut buf = [0; 6];
        while qsize > 0 {
            qsize -= 1;
            let node = queue[qsize];
            if visited.get(node) {
                continue;
            }
            visited.set(node);
            if node != orig {
                turns.push(Turn::Move(orig, node));
            }
            for adj in self.slidable_adjacent(&mut buf, orig, node) {
                if !visited.get(adj) {
                    queue[qsize] = adj;
                    qsize += 1;
                }
            }
        }
    }

    fn generate_ladybug(&self, hex: Hex, turns: &mut Vec<Turn>) {
        let mut buf1 = [0; 6];
        let mut buf2 = [0; 6];
        let mut buf3 = [0; 6];
        let mut step2 = HexSet::new();
        let mut step3 = HexSet::new();
        for s1 in self.slidable_adjacent_beetle(&mut buf1, hex, hex) {
            if self.occupied(s1) {
                for s2 in self.slidable_adjacent_beetle(&mut buf2, hex, s1) {
                    if self.occupied(s2) && !step2.get(s2) {
                        step2.set(s2);
                        for s3 in self.slidable_adjacent_beetle(&mut buf3, hex, s2) {
                            if !self.occupied(s3) && !step3.get(s3) {
                                step3.set(s3);
                                turns.push(Turn::Move(hex, s3));
                            }
                        }
                    }
                }
            }
        }
    }

    fn generate_throws(
        &self, immovable: &HexSet, hex: Hex, turns: &mut Vec<Turn>, throw_starts: &mut HexSet,
        throw_ends: &mut HexSet,
    ) {
        let mut starts = [0; 6];
        let mut num_starts = 0;
        let mut ends = [0; 6];
        let mut num_ends = 0;
        let mut buf = [0; 6];
        let origin = Direction::NW.apply(Direction::NW.apply(hex)); // something not adjacent
        for adj in self.slidable_adjacent_beetle(&mut buf, origin, hex) {
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
                turns.push(Turn::Move(start, end));
                throw_starts.set(start);
                throw_ends.set(end);
            }
        }
    }

    fn generate_mosquito(&self, hex: Hex, turns: &mut Vec<Turn>) {
        let mut targets = [false; 8];
        for adj in adjacent(hex) {
            let node = self.nodes[adj as usize];
            if node.occupied() {
                targets[node.bug() as usize] = true;
            }
        }

        let mut i = turns.len();
        if targets[Bug::Ant as usize] {
            self.generate_walk_all(hex, turns);
        } else {
            // Avoid adding strictly duplicative moves to the ant.
            if targets[Bug::Queen as usize]
                || targets[Bug::Beetle as usize]
                || targets[Bug::Pillbug as usize]
            {
                self.generate_walk1(hex, turns);
            }
            if targets[Bug::Spider as usize] {
                self.generate_walk3(hex, turns);
            }
        }
        if targets[Bug::Grasshopper as usize] {
            self.generate_jumps(hex, turns);
        }
        if targets[Bug::Beetle as usize] {
            self.generate_stack_walking(hex, turns);
        }
        if targets[Bug::Ladybug as usize] {
            self.generate_ladybug(hex, turns);
        }

        // Remove duplicates.
        let mut dests = HexSet::new();
        while i < turns.len() {
            if let Turn::Move(_, dest) = turns[i] {
                if dests.get(dest) {
                    turns.swap_remove(i);
                } else {
                    dests.set(dest);
                    i += 1;
                }
            }
        }
    }

    pub(crate) fn generate_movements(&self, turns: &mut Vec<Turn>) {
        let mut immovable = self.find_cut_vertexes();
        let stunned = match self.turn_history.last() {
            Some(Turn::Move(_, dest)) => Some(dest),
            _ => None,
        };
        if let Some(moved) = stunned {
            // Can't move pieces that were moved on the opponent's turn.
            immovable.set(*moved);
        }

        // Pillbug throws need to be deduped against organic movements, so generate them first.
        let mut throw_starts = HexSet::new();
        let mut throw_ends = HexSet::new();
        let first_move = turns.len();
        let mut marker;
        for &hex in self.occupied_hexes[self.to_move() as usize].iter() {
            marker = turns.len();
            let node = self.node(hex);
            if stunned == Some(&hex) {
                continue;
            }
            if node.bug() == Bug::Pillbug
                || (node.bug() == Bug::Mosquito
                    && !node.is_stacked()
                    && adjacent(hex).iter().any(|&adj| {
                        let n = self.node(adj);
                        n.occupied() && n.bug() == Bug::Pillbug
                    }))
            {
                self.generate_throws(&immovable, hex, turns, &mut throw_starts, &mut throw_ends);
                // Dedup throws from pillbug and mosquito
                if marker > 0 {
                    let mut i = marker;
                    while i < turns.len() {
                        if turns[first_move..marker].contains(&turns[i]) {
                            turns.swap_remove(i);
                        } else {
                            i += 1;
                        }
                    }
                }
            }
        }
        let num_throws = turns.len();

        for &hex in self.occupied_hexes[self.to_move() as usize].iter() {
            marker = turns.len();
            let node = self.node(hex);
            if node.is_stacked() {
                self.generate_stack_walking(hex, turns);
                continue;
            }
            if immovable.get(hex) {
                continue;
            }
            match node.bug() {
                Bug::Queen => self.generate_walk1(hex, turns),
                Bug::Grasshopper => self.generate_jumps(hex, turns),
                Bug::Spider => self.generate_walk3(hex, turns),
                Bug::Ant => self.generate_walk_all(hex, turns),
                Bug::Beetle => {
                    self.generate_walk1(hex, turns);
                    self.generate_stack_walking(hex, turns);
                }
                Bug::Mosquito => self.generate_mosquito(hex, turns),
                Bug::Ladybug => self.generate_ladybug(hex, turns),
                Bug::Pillbug => self.generate_walk1(hex, turns),
            }

            // Dedup against pillbug throws.
            if throw_starts.get(hex) {
                let mut i = marker;
                while i < turns.len() {
                    let turn = turns[i];
                    let end = match turn {
                        Turn::Move(_, end) => end,
                        _ => {
                            i += 1;
                            continue;
                        }
                    };
                    if throw_ends.get(end) && turns[first_move..num_throws].contains(&turn) {
                        turns.swap_remove(i);
                    } else {
                        i += 1;
                    }
                }
            }
        }
    }
}

pub struct Rules;

impl minimax::Game for Rules {
    type S = Board;
    type M = Turn;

    fn generate_moves(board: &Board, turns: &mut Vec<Turn>) {
        if board.turn_num < 2 {
            // Special case for the first 2 turns:
            for (bug, num_left) in board.get_available_bugs().iter() {
                if *bug == Bug::Queen {
                    // To reduce draws, implement tournament rule where
                    // you can't place your queen first.
                    continue;
                }
                if *num_left > 0 {
                    if board.turn_num == 0 {
                        turns.push(Turn::Place(START_HEX, *bug));
                    } else {
                        for &hex in adjacent(START_HEX).iter() {
                            turns.push(Turn::Place(hex, *bug));
                        }
                    }
                }
            }
            return;
        }

        // Once queen has been placed, pieces may move.
        if board.get_remaining()[Bug::Queen as usize] == 0 {
            // For movable pieces, generate all legal moves.
            board.generate_movements(turns);
        }

        if board.get_remaining().iter().any(|&num| num > 0) {
            // Find placeable positions.
            board.generate_placements(turns);
        }

        if turns.is_empty() {
            turns.push(Turn::Pass);
        }
    }

    fn get_winner(board: &Board) -> Option<minimax::Winner> {
        let queens_surrounded = board.queens_surrounded();
        let n = board.zobrist_history.len();
        if n > 10 {
            // Check for position repeat stalemate.
            // More than 32 turns ago, we're not going to bother looking.
            // Check every 4 turns as both players need to move and move back to repeat.
            // Last turn is at zobrist_history[n-1], so offset by 1.
            let start = if n < 35 { (n - 1) % 4 } else { n - 33 };
            let recent_past = &board.zobrist_history[start..n];
            let position_repeat_count =
                recent_past.iter().step_by(4).filter(|&&hash| hash == board.zobrist_hash).count();
            if position_repeat_count >= 2 {
                // Draw by stalemate.
                return Some(minimax::Winner::Draw);
            }
        }

        if queens_surrounded == [6, 6] {
            // Draw by simultaneous queen surrounding.
            Some(minimax::Winner::Draw)
        } else if queens_surrounded[board.to_move() as usize] == 6 {
            Some(minimax::Winner::PlayerJustMoved)
        } else if queens_surrounded[board.to_move().other()] == 6 {
            Some(minimax::Winner::PlayerToMove)
        } else {
            None
        }
    }

    fn apply(board: &mut Board, turn: Turn) -> Option<Board> {
        board.apply(turn);
        None
    }

    fn undo(board: &mut Board, turn: Turn) {
        board.undo(turn);
    }

    fn zobrist_hash(board: &Board) -> u64 {
        board.zobrist_hash
    }

    fn null_move(_: &Board) -> Option<Turn> {
        Some(Turn::Pass)
    }

    fn notation(board: &Board, turn: Turn) -> Option<String> {
        Some(board.to_move_string(turn))
    }

    fn table_index(turn: Turn) -> u16 {
        // Arbitarily squeezed smaller by shaving off
        // the highest bit from each hex.
        const MASK: u16 = 0x7f;
        match turn {
            Turn::Place(hex, bug) => (bug as u16) << 7 | hex as u16 & MASK,
            Turn::Move(start, end) => (start as u16 & MASK) << 7 | end as u16 & MASK,
            Turn::Pass => 0,
        }
    }
    /// Maximum table size.
    fn max_table_index() -> u16 {
        u16::MAX >> 2
    }
}

// Coordinates for populating test positions.
pub(crate) type Loc = (i8, i8);
pub fn loc_to_hex(loc: Loc) -> Hex {
    // Centered in the middle of the board.
    START_HEX.wrapping_add(ROW_SIZE.wrapping_mul(loc.1 as Hex)).wrapping_add(loc.0 as Hex)
}

#[cfg(test)]
pub(crate) fn hex_to_loc(hex: Hex) -> Loc {
    let mut x = (hex.wrapping_sub(START_HEX - ROW_SIZE / 2) / ROW_SIZE) as i8;
    if x > 7 {
        x -= ROW_SIZE as i8;
    }
    let mut y = (hex.wrapping_sub(START_HEX) % ROW_SIZE) as i8;
    if y > 7 {
        y -= ROW_SIZE as i8;
    }
    (y, x)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex_loc() {
        for x in -6..6 {
            for y in -6..6 {
                let loc = (x, y);
                let hex = loc_to_hex(loc);
                assert_eq!(loc, hex_to_loc(hex), "{}", hex);
            }
        }
    }

    impl Board {
        fn insert_loc(&mut self, loc: Loc, bug: Bug, color: Color) {
            self.insert(loc_to_hex(loc), bug, 0, color);
        }

        fn remove_loc(&mut self, loc: Loc) {
            self.remove(loc_to_hex(loc));
        }

        fn fill_board(&mut self, locs: &[Loc], bug: Bug) {
            for &loc in locs {
                self.insert(loc_to_hex(loc), bug, 0, Color::Black);
            }
        }

        fn assert_placements(&self, turns: &[Turn], expected: &[(Loc, Bug)]) {
            let mut actual_pairs = Vec::new();
            for &m in turns.iter() {
                if let Turn::Place(actual_hex, actual_bug) = m {
                    actual_pairs.push((hex_to_loc(actual_hex), actual_bug));
                }
            }
            actual_pairs.sort();
            let mut expected_pairs = Vec::new();
            expected_pairs.extend(expected);
            expected_pairs.sort();
            assert_eq!(actual_pairs, expected_pairs);
        }

        fn assert_movements(&self, turns: &[Turn], start: Loc, ends: &[Loc]) {
            let mut actual_ends = Vec::new();
            for &m in turns.iter() {
                if let Turn::Move(actual_start, actual_end) = m {
                    if hex_to_loc(actual_start) == start {
                        actual_ends.push(hex_to_loc(actual_end));
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
        board.insert_loc((0, 0), Bug::Queen, Color::White);
        board.insert_loc((1, 0), Bug::Queen, Color::Black);
        let mut turns = Vec::new();
        board.generate_placements(&mut turns);
        board.assert_placements(
            &turns,
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
        for hex in 0..GRID_MASK {
            if cuts.get(hex) {
                cut_locs.push(hex_to_loc(hex));
            }
        }
        assert_eq!(&[(-1, 0), (2, 1)], &cut_locs[..]);
    }

    #[test]
    fn test_slidable() {
        let mut board = Board::default();
        let x = START_HEX;
        let mut buf = [0; 6];
        // One neighbor.
        board.insert_loc((0, 0), Bug::Queen, Color::Black);
        board.insert_loc((1, 0), Bug::Queen, Color::Black);
        assert_eq!(
            vec![loc_to_hex((0, -1)), loc_to_hex((1, 1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Hex>>()
        );
        // Two adjacent neighbors.
        board.insert_loc((1, 1), Bug::Queen, Color::Black);
        assert_eq!(
            vec![loc_to_hex((0, -1)), loc_to_hex((0, 1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Hex>>()
        );
        // Four adjacent neighbors.
        board.insert_loc((0, 1), Bug::Queen, Color::Black);
        board.insert_loc((-1, 0), Bug::Queen, Color::Black);
        assert_eq!(
            vec![loc_to_hex((-1, -1)), loc_to_hex((0, -1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Hex>>()
        );
        // Five adjacent neighbors.
        board.insert_loc((-1, -1), Bug::Queen, Color::Black);
        assert_eq!(
            Vec::<Hex>::new(),
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Hex>>()
        );
        // 2 separated groups of neighbors.
        board.remove_loc((0, 1));
        assert_eq!(
            Vec::<Hex>::new(),
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Hex>>()
        );
        // 2 opposite single neighbors
        board.remove_loc((1, 1));
        board.remove_loc((-1, -1));
        assert_eq!(
            vec![loc_to_hex((-1, -1)), loc_to_hex((0, -1)), loc_to_hex((1, 1)), loc_to_hex((0, 1))],
            board.slidable_adjacent(&mut buf, x, x).collect::<Vec<Hex>>()
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
        let mut turns = Vec::new();
        board.generate_jumps(START_HEX, &mut turns);
        board.assert_movements(&turns, (0, 0), &[(0, 2), (3, 0)]);
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
        let mut turns = Vec::new();
        board.generate_stack_walking(START_HEX, &mut turns);
        board.assert_movements(&turns, (0, 0), &[(-1, -1), (0, -1), (0, 1), (1, 1)]);
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
        let mut turns = Vec::new();
        let start = loc_to_hex((-1, -1));
        board.generate_walk3(start, &mut turns);
        board.assert_movements(&turns, (-1, -1), &[(0, 2), (1, -1), (1, 1), (2, 1)]);

        // ．．🕷．🕷．．
        //．．🕷🕷．🕷．
        // ．．🕷🕷🕷
        board.remove_loc((-1, -1));
        board.insert_loc((1, 1), Bug::Spider, Color::Black);
        turns.clear();
        let start = loc_to_hex((1, 1));
        board.generate_walk3(start, &mut turns);
        board.assert_movements(&turns, (1, 1), &[(-1, -1), (0, -1), (1, -1), (2, -1)]);
    }

    #[test]
    fn test_generate_walk_all() {
        let mut board = Board::default();
        //．．．🐜．．．．
        // ．．．🐜．．．
        //．．．🐜．🐜．
        // ．．．🐜🐜
        board.fill_board(&[(-1, -1), (0, 0), (0, 1), (2, 1), (1, 2), (2, 2)], Bug::Ant);
        let mut turns = Vec::new();
        let start = loc_to_hex((-1, -1));
        board.generate_walk_all(start, &mut turns);
        board.assert_movements(
            &turns,
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
        let mut turns = Vec::new();
        board.generate_mosquito(loc_to_hex((0, 0)), &mut turns);
        // Mosquito on mosquito can't move at all.
        board.assert_movements(&turns, (0, 0), &[]);

        //．．🦟🦗．
        // ．🐜🪲．
        board.insert_loc((0, 1), Bug::Ant, Color::Black);
        board.insert_loc((1, 1), Bug::Beetle, Color::Black);
        board.insert_loc((1, 0), Bug::Grasshopper, Color::Black);
        turns.clear();
        // Dedup happens in generate_movements.
        board.turn_num += 1;
        board.generate_movements(&mut turns);
        board.assert_movements(
            &turns,
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
        let mut turns = Vec::new();
        let start = loc_to_hex((2, 3));
        board.generate_ladybug(start, &mut turns);
        board.assert_movements(
            &turns,
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
        let mut turns = Vec::new();
        let immovable = HexSet::new();
        let mut starts = HexSet::new();
        let mut ends = HexSet::new();
        let start = loc_to_hex((1, 1));
        board.generate_throws(&immovable, start, &mut turns, &mut starts, &mut ends);
        assert_eq!(4, turns.len());
        board.assert_movements(&turns[..2], (1, 2), &[(1, 0), (2, 1)]);
        board.assert_movements(&turns[2..], (0, 1), &[(1, 0), (2, 1)]);

        // Create a level-2 gate to prevent one piece from being thrown.
        board.remove_loc((0, 0));
        board.insert_loc((0, 1), Bug::Pillbug, Color::Black);
        turns.clear();
        board.generate_throws(&immovable, start, &mut turns, &mut starts, &mut ends);
        assert_eq!(2, turns.len());
        board.assert_movements(&turns, (0, 0), &[(1, 0), (2, 1)]);

        // Create a level-2 gate to prevent one destination to being thrown to.
        board.insert_loc((1, 0), Bug::Pillbug, Color::Black);
        board.insert_loc((1, 0), Bug::Pillbug, Color::Black);
        board.remove_loc((0, 1));
        board.remove_loc((0, 1));
        board.remove_loc((1, 2));
        turns = Vec::new();
        board.generate_throws(&immovable, start, &mut turns, &mut starts, &mut ends);
        assert_eq!(2, turns.len());
        board.assert_movements(&turns, (0, 0), &[(0, 1), (1, 2)]);
    }

    #[test]
    fn test_winner() {
        use minimax::Game;

        // Draw by threefold repetition.
        let mut board = Board::default();
        let x1 = loc_to_hex((-1, -1));
        let x2 = loc_to_hex((-1, 0));
        let y1 = loc_to_hex((1, 1));
        let y2 = loc_to_hex((1, 0));
        board.apply(Turn::Place(loc_to_hex((0, 0)), Bug::Spider));
        assert_eq!(None, Rules::get_winner(&board));
        board.apply(Turn::Place(x1, Bug::Queen));
        assert_eq!(None, Rules::get_winner(&board));
        // Create the position the first time.
        board.apply(Turn::Place(y1, Bug::Queen));
        assert_eq!(None, Rules::get_winner(&board));
        board.apply(Turn::Move(x1, x2));
        assert_eq!(None, Rules::get_winner(&board));
        board.apply(Turn::Move(y1, y2));
        assert_eq!(None, Rules::get_winner(&board));
        board.apply(Turn::Move(x2, x1));
        assert_eq!(None, Rules::get_winner(&board));
        // Recreate position for the second time.
        board.apply(Turn::Move(y2, y1));
        assert_eq!(None, Rules::get_winner(&board));
        board.apply(Turn::Move(x1, x2));
        assert_eq!(None, Rules::get_winner(&board));
        board.apply(Turn::Move(y1, y2));
        assert_eq!(None, Rules::get_winner(&board));
        board.apply(Turn::Move(x2, x1));
        assert_eq!(None, Rules::get_winner(&board));
        // Recreate position for the third time.
        board.apply(Turn::Move(y2, y1));
        assert_eq!(Some(minimax::Winner::Draw), Rules::get_winner(&board));
        // Undo reverts zobrist and history.
        board.undo(Turn::Move(y2, y1));
        assert_eq!(None, Rules::get_winner(&board));
        // Redo re-reverts draw state.
        board.apply(Turn::Move(y2, y1));
        assert_eq!(Some(minimax::Winner::Draw), Rules::get_winner(&board));
    }
}
