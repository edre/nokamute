extern crate minimax;
extern crate termcolor;

use crate::bug::Bug;
use crate::hex_grid::*;
use std::borrow::Borrow;
use std::cmp::{max, min};
use std::collections::hash_map::DefaultHasher;
use std::default::Default;
use std::hash::Hasher;
use std::io::Write;
use termcolor::WriteColor;

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
        unsafe { std::mem::transmute::<u8, Color>(self.0 as u8 >> 7) }
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

    fn clipped_height(self) -> u8 {
        self.0 & 0x3
    }
}

#[derive(Copy, Clone)]
pub struct UnderNode {
    // What bug is here.
    node: Node,
    // Where is this stack.
    id: Id,
    // What is the height of this piece.
    height: u8,
}

impl UnderNode {
    fn new(node: Node, id: Id, height: u8) -> Self {
        Self { node: node.with_height(height), id, height }
    }

    fn empty() -> Self {
        Self { node: Node::empty(), id: 0, height: 0 }
    }

    pub fn node(&self) -> Node {
        self.node
    }
    pub fn id(&self) -> Id {
        self.id
    }
}

#[derive(Clone)]
pub struct Board {
    // Indexed by Id.
    pub(crate) nodes: [Node; GRID_SIZE],
    // Tiles that are under other tiles.
    // Sorted by height (for each id) and no gaps.
    underworld: [UnderNode; 8],
    underworld_size: usize,
    pub(crate) remaining: [[u8; 8]; 2],
    pub(crate) queens: [Id; 2],
    pub(crate) occupied_ids: [Vec<Id>; 2],

    pub(crate) move_num: u16,
    zobrist_table: &'static [u64; GRID_SIZE * 2],
    zobrist_hash: u64,
    zobrist_history: Vec<u64>,
    // Board history.
    pub(super) move_history: Vec<Move>,

    pub(super) game_type_bits: u8,
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

    pub(crate) fn node(&self, id: Id) -> Node {
        self.nodes[id as usize]
    }

    fn zobrist(&self, id: Id, bug: Bug, color: Color, height: u8) -> u64 {
        let hash = self.zobrist_table[(id as usize) << 1 | (color as usize)];
        // I don't really want to multiply the table by another factor of 7*8, so
        // just realign the existing random bits.
        // Also include the color to move hash.
        hash.rotate_left(((height as u32) << 3) | bug as u32)
    }

    pub fn get_underworld(&self) -> &[UnderNode] {
        &self.underworld[0..self.underworld_size]
    }

    fn insert_underworld(&mut self, node: Node, id: Id) {
        let height = self.underworld_height(id, node);
        if self.underworld_size >= self.underworld.len() {
            unreachable!("underworld overflowed");
        }
        self.underworld[self.underworld_size] = UnderNode::new(node, id, height);
        self.underworld_size += 1;
    }

    fn remove_underworld(&mut self, id: Id) -> Node {
        for i in (0..self.underworld_size).rev() {
            if self.underworld[i].id == id {
                let node = self.underworld[i].node;
                self.underworld[i..self.underworld_size].rotate_left(1);
                self.underworld_size -= 1;
                return node;
            }
        }
        unreachable!("underworld not found");
    }

    fn occupied_add(&mut self, color: Color, id: Id) {
        self.occupied_ids[color as usize].push(id);
    }

    fn occupied_remove(&mut self, color: Color, id: Id) {
        let vec = &mut self.occupied_ids[color as usize];
        let i = vec.iter().position(|&x| x == id).unwrap();
        vec.swap_remove(i);
    }

    fn insert(&mut self, id: Id, bug: Bug, bug_num: u8, color: Color) {
        let prev = self.node(id);
        if prev.occupied() {
            if prev.color() != color {
                self.occupied_remove(prev.color(), id);
                self.occupied_add(color, id);
            }
        } else {
            self.occupied_add(color, id);
        }
        if prev.occupied() {
            self.insert_underworld(prev, id);
        }
        let clipped_height = min(prev.clipped_height() + 1, 3);
        self.nodes[id as usize] = Node::new_occupied(bug, color, bug_num, clipped_height);
        self.zobrist_hash ^= self.zobrist(id, bug, color, self.height(id));

        if bug == Bug::Queen {
            self.queens[color as usize] = id;
        }
    }

    // Asserts that there is something there.
    fn remove(&mut self, id: Id) -> (Bug, u8, Color) {
        let height = self.height(id);
        let prev = self.node(id);

        let new_node = if height > 1 { self.remove_underworld(id) } else { Node::empty() };
        self.nodes[id as usize] = new_node;
        let bug = prev.bug();
        let color = prev.color();
        if new_node.occupied() {
            if new_node.color() != color {
                self.occupied_remove(color, id);
                self.occupied_add(new_node.color(), id);
            }
        } else {
            self.occupied_remove(color, id);
        }

        self.zobrist_hash ^= self.zobrist(id, bug, color, height);
        if bug == Bug::Queen {
            self.queens[color as usize] = START_ID;
        }
        (bug, prev.bug_num(), color)
    }

    fn underworld_height(&self, id: Id, node: Node) -> u8 {
        let height = node.clipped_height();
        if height > 2 {
            1 + self.underworld[0..self.underworld_size]
                .iter()
                .rev()
                .find(|under| under.id == id)
                .unwrap()
                .height
        } else {
            height
        }
    }

    fn height(&self, id: Id) -> u8 {
        self.underworld_height(id, self.node(id))
    }

    pub(crate) fn occupied(&self, id: Id) -> bool {
        self.node(id).occupied()
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

    pub(crate) fn queen_required(&self) -> bool {
        self.move_num > 5 && self.get_remaining()[Bug::Queen as usize] > 0
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
            queens: [START_ID; 2],
            occupied_ids: [Vec::new(), Vec::new()],
            move_num: 0,
            zobrist_table: ZOBRIST_TABLE.borrow(),
            zobrist_hash: 0,
            zobrist_history: Vec::new(),
            move_history: Vec::new(),
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

impl Board {
    // return the Id to the upper left and lower right of all occupied nodes.
    // Given wrapping, the second may be less than the first.
    fn bounding_box(&self) -> (Id, Id, Id, Id) {
        let empty_rows = (0..ROW_SIZE)
            .map(|r| ((0..ROW_SIZE).all(|c| !self.occupied(r * ROW_SIZE + c))))
            .collect::<Vec<bool>>();
        let empty_cols = (0..ROW_SIZE)
            .map(|c| ((0..ROW_SIZE).all(|r| !self.occupied(r * ROW_SIZE + c))))
            .collect::<Vec<bool>>();
        if empty_rows.iter().all(|&r| r) {
            // Center around start id
            return (
                START_ID / ROW_SIZE - 1,
                START_ID / ROW_SIZE + 1,
                START_ID % ROW_SIZE - 1,
                START_ID % ROW_SIZE + 1,
            );
        }
        let mut minr = 0;
        let mut maxr = 0;
        let mut minc = 0;
        let mut maxc = 0;
        for i in 0..ROW_SIZE as usize {
            let j = (i + 1) % ROW_SIZE as usize;
            if empty_rows[i] && !empty_rows[j] {
                minr = i as Id;
            }
            if !empty_rows[i] && empty_rows[j] {
                maxr = j as Id;
            }
            if empty_cols[i] && !empty_cols[j] {
                minc = i as Id;
            }
            if !empty_cols[i] && empty_cols[j] {
                maxc = j as Id;
            }
        }
        (minr, maxr, minc, maxc)
    }

    pub fn fancy_fmt(&self, buf: &mut termcolor::Buffer, highlights: &[Id]) -> std::io::Result<()> {
        let (startr, endr, startc, endc) = self.bounding_box();
        let free_space = "\u{ff0e}".as_bytes();

        let mut r = startr;
        while r != (endr + 1) % ROW_SIZE {
            // Print prefix to get staggered hex rows
            let buflen = endr.wrapping_sub(r) % ROW_SIZE;
            if buflen % 2 == 1 {
                buf.write_all(b" ")?;
            }
            for _ in 0..buflen / 2 {
                buf.write_all(free_space)?;
            }

            let mut c = startc;
            while c != (endc + 1) % ROW_SIZE {
                let id = c + r * ROW_SIZE;
                if let Some(index) = highlights.iter().position(|&x| x == id) {
                    write!(buf, "{: >2}", index)?;
                    c = (c + 1) % ROW_SIZE;
                    continue;
                }
                let node = self.node(id);
                if node.occupied() {
                    if node.color() == Color::White {
                        // Invert terminal background color for white pieces.
                        buf.set_color(
                            termcolor::ColorSpec::new().set_bg(Some(termcolor::Color::White)),
                        )?;
                    }
                    write!(buf, "{}", node.bug().codepoint())?;
                    if node.color() == Color::White {
                        // Reset coloring.
                        buf.reset()?;
                    }
                } else {
                    // Empty cell. Full width period.
                    buf.write_all(free_space)?;
                }
                c = (c + 1) % ROW_SIZE;
            }

            // Stagger rows the other way to make the space look rectangular.
            for _ in 0..r.wrapping_sub(startr) % ROW_SIZE / 2 {
                buf.write_all(free_space)?;
            }

            // On 2nd and 3rd rows, print remaining bugs from each side
            if r == (startr + 1) % ROW_SIZE {
                buf.write_all(b" ")?;
                self.write_remaining(Color::White, buf)?;
            } else if r == (startr + 2) % ROW_SIZE {
                self.write_remaining(Color::Black, buf)?;
            }

            buf.write_all(b"\n")?;
            r = (r + 1) % ROW_SIZE;
        }
        Ok(())
    }

    fn write_remaining(&self, color: Color, buf: &mut termcolor::Buffer) -> std::io::Result<()> {
        for bug in Bug::iter_all() {
            let count = self.remaining[color as usize][bug as usize];
            let prefix = if count == 2 {
                b"2"
            } else if count == 3 {
                b"3"
            } else {
                b" "
            };
            if count > 0 {
                buf.write_all(prefix)?;
                if color == Color::White {
                    // Invert terminal background color for white pieces.
                    buf.set_color(
                        termcolor::ColorSpec::new().set_bg(Some(termcolor::Color::White)),
                    )?;
                }
                write!(buf, "{}", bug.codepoint())?;
                if color == Color::White {
                    // Reset coloring.
                    buf.reset()?;
                }
                buf.write_all(b" ")?;
            }
        }
        Ok(())
    }

    pub(crate) fn println(&self) {
        self.println_highlights(&[]);
    }

    pub(crate) fn println_highlights(&self, highlights: &[Id]) {
        let writer = termcolor::BufferWriter::stdout(termcolor::ColorChoice::Auto);
        let mut buffer = writer.buffer();
        self.fancy_fmt(&mut buffer, highlights).unwrap();
        writer.print(&buffer).unwrap();
    }
}

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
        match *self {
            Move::Place(id, bug) => {
                let bug_num =
                    Bug::initial_quantity()[bug as usize] - board.get_remaining()[bug as usize] + 1;
                board.insert(id, bug, bug_num, board.to_move());
                board.mut_remaining()[bug as usize] -= 1;
            }
            Move::Movement(start, end) => {
                let (bug, bug_num, color) = board.remove(start);
                board.insert(end, bug, bug_num, color);
                // Encode a marker of last moved location for pillbug throwability.
                board.zobrist_hash ^= end as u64;
            }
            _ => {}
        };
        board.move_num += 1;
        // Encode positions differently based on who is to move.
        board.zobrist_hash ^= 0xa6c11b626b105b7c;
        // Undo last-moved zobrist bits.
        if let Some(Move::Movement(_, end)) = board.move_history.last() {
            board.zobrist_hash ^= *end as u64;
        }
        board.zobrist_history.push(board.zobrist_hash);
        board.move_history.push(*self);
    }

    fn undo(&self, board: &mut Board) {
        board.move_num -= 1;
        board.zobrist_history.pop();
        board.move_history.pop();
        if let Some(Move::Movement(_, end)) = board.move_history.last() {
            board.zobrist_hash ^= *end as u64;
        }
        match *self {
            Move::Place(id, bug) => {
                board.remove(id);
                board.mut_remaining()[bug as usize] += 1;
            }
            Move::Movement(start, end) => {
                let (bug, bug_num, color) = board.remove(end);
                board.insert(start, bug, bug_num, color);
                board.zobrist_hash ^= end as u64;
            }
            Move::Pass => {}
        }
        // Encode positions differently based on who is to move.
        board.zobrist_hash ^= 0xa6c11b626b105b7c;
    }

    fn notation(&self, board: &Board) -> Option<String> {
        Some(board.to_move_string(*self))
    }
}

impl Board {
    fn generate_placements(&self, moves: &mut Vec<Move>) {
        let mut enemy_adjacent = NodeSet::new();
        for &enemy in self.occupied_ids[self.to_move().other()].iter() {
            for adj in adjacent(enemy) {
                enemy_adjacent.set(adj);
            }
        }

        let mut visited = NodeSet::new();
        for &friend in self.occupied_ids[self.to_move() as usize].iter() {
            for id in adjacent(friend) {
                if visited.get(id) {
                    continue;
                }
                visited.set(id);
                if self.occupied(id) || enemy_adjacent.get(id) {
                    continue;
                }
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
    // DFS iteration order is important.
    // Perhaps you can find the first-explored neighbor and restart the DFS search at that point?
    // This would be very fast for repeated insertion/removal of leaf nodes (Placement heavy parts of search).
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
        for dir in Direction::all() {
            let mut jump = dir.apply(id);
            let mut dist = 1;
            while self.occupied(jump) {
                jump = dir.apply(jump);
                dist += 1;
                if jump == id {
                    // Exit out if we'd infinitey loop.
                    dist = 0;
                    break;
                }
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
        fn dfs(
            id: Id, orig: Id, board: &Board, path: &mut Vec<Id>, visited: &mut NodeSet,
            moves: &mut Vec<Move>,
        ) {
            if path.contains(&id) {
                return;
            }
            if path.len() == 3 {
                if !visited.get(id) {
                    moves.push(Move::Movement(orig, id));
                    visited.set(id);
                }
                return;
            }
            path.push(id);
            let mut buf = [0; 6];
            for adj in board.slidable_adjacent(&mut buf, orig, id) {
                dfs(adj, orig, board, path, visited, moves);
            }
            path.pop();
        }
        let mut path = Vec::with_capacity(3);
        let mut visited = NodeSet::new();
        dfs(orig, orig, self, &mut path, &mut visited, moves);
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

    fn generate_throws(&self, immovable: &NodeSet, id: Id, moves: &mut Vec<Move>) -> bool {
        let mut starts = [0; 6];
        let mut num_starts = 0;
        let mut ends = [0; 6];
        let mut num_ends = 0;
        let mut buf = [0; 6];
        let origin = Direction::NW.apply(Direction::NW.apply(id)); // something not adjacent
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
        num_starts > 0 && num_ends > 0
    }

    fn generate_mosquito(&self, id: Id, moves: &mut Vec<Move>) {
        let mut targets = [false; 8];
        for adj in adjacent(id) {
            let node = self.nodes[adj as usize];
            if node.occupied() {
                targets[node.bug() as usize] = true;
            }
        }

        let mut i = moves.len();
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

        // Remove duplicates.
        let mut dests = NodeSet::new();
        while i < moves.len() {
            if let Move::Movement(_, dest) = moves[i] {
                if dests.get(dest) {
                    moves.swap_remove(i);
                } else {
                    dests.set(dest);
                    i += 1;
                }
            }
        }
    }

    pub(crate) fn generate_movements(&self, moves: &mut Vec<Move>) {
        let mut immovable = self.find_cut_vertexes();
        let stunned = match self.move_history.last() {
            Some(Move::Movement(_, dest)) => Some(dest),
            _ => None,
        };
        if let Some(moved) = stunned {
            // Can't move pieces that were moved on the opponent's turn.
            immovable.set(*moved);
        }

        let mut dedup = false;
        for &id in self.occupied_ids[self.to_move() as usize].iter() {
            let node = self.node(id);
            if node.is_stacked() {
                self.generate_stack_walking(id, moves);
                // Don't let mosquito on stack use pillbug ability.
                // Although the rules don't seem to specify either way.
                continue;
            }
            // Check for throw ability before movability, as pinned pillbugs can still throw.
            let pillbug_powers = node.bug() == Bug::Pillbug
                || (node.bug() == Bug::Mosquito
                    && adjacent(id).iter().any(|&adj| {
                        let n = self.node(adj);
                        n.occupied() && n.bug() == Bug::Pillbug
                    }));
            // However pillbugs just thrown cannot throw.
            if pillbug_powers && stunned != Some(&id) {
                dedup |= self.generate_throws(&immovable, id, moves);
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
                Bug::Mosquito => self.generate_mosquito(id, moves),
                Bug::Ladybug => self.generate_ladybug(id, moves),
                Bug::Pillbug => self.generate_walk1(id, moves),
            }
        }

        if dedup {
            // Mosquitos and pillbugs can create duplicate moves, so sort and dedup.
            // TODO: Maybe only enable this for perft?
            // Dups get resolved quickly with the transposition table and this dedup is very slow.
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
        let n = board.zobrist_history.len();
        if n > 10 {
            // Check for position repeat stalemate.
            // More than 32 moves ago, we're not going to bother looking.
            // Check every 4 moves as both players need to move and move back to repeat.
            // Last move is at zobrist_history[n-1], so offset by 1.
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

    fn null_move(_: &Board) -> Option<Move> {
        Some(Move::Pass)
    }
}

// Coordinates for populating test positions.
pub(crate) type Loc = (i8, i8);
pub fn loc_to_id(loc: Loc) -> Id {
    // Centered in the middle of the board.
    START_ID.wrapping_add(ROW_SIZE.wrapping_mul(loc.1 as Id)).wrapping_add(loc.0 as Id)
}

#[cfg(test)]
pub(crate) fn id_to_loc(id: Id) -> Loc {
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
        for x in -6..6 {
            for y in -6..6 {
                let loc = (x, y);
                let id = loc_to_id(loc);
                assert_eq!(loc, id_to_loc(id), "{}", id);
            }
        }
    }

    impl Board {
        fn insert_loc(&mut self, loc: Loc, bug: Bug, color: Color) {
            self.insert(loc_to_id(loc), bug, 0, color);
        }

        fn remove_loc(&mut self, loc: Loc) {
            self.remove(loc_to_id(loc));
        }

        fn fill_board(&mut self, locs: &[Loc], bug: Bug) {
            for &loc in locs {
                self.insert(loc_to_id(loc), bug, 0, Color::Black);
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
        board.insert_loc((0, 0), Bug::Queen, Color::White);
        board.insert_loc((1, 0), Bug::Queen, Color::Black);
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
        for id in 0..GRID_MASK {
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

        // Draw by threefold repetition.
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
        // Redo re-reverts draw state.
        super::Move::Movement(y2, y1).apply(&mut board);
        assert_eq!(Some(minimax::Winner::Draw), Rules::get_winner(&board));
    }
}
