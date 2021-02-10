extern crate minimax;

use minimax::*;
use std::cmp::{max, min};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

fn timeout_signal(dur: Duration) -> Arc<AtomicBool> {
    // Theoretically we could include an async runtime to do this and use
    // fewer threads, but the stdlib implementation is only a few lines...
    let signal = Arc::new(AtomicBool::new(false));
    let signal2 = signal.clone();
    std::thread::spawn(move || {
        std::thread::sleep(dur);
        signal2.store(true, Ordering::Relaxed);
    });
    signal
}

pub trait Zobrist {
    // Hash of the game position.
    fn zobrist_hash(&self) -> u64;
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum EntryFlag {
    Exact,
    Upperbound,
    Lowerbound,
}

#[derive(Copy, Clone)]
struct Entry<M> {
    hash: u64,
    value: minimax::Evaluation,
    depth: u8,
    flag: EntryFlag,
    best_move: M,
}

impl<M> Default for Entry<M>
where
    M: Default,
{
    fn default() -> Self {
        Entry::<M> {
            hash: 0,
            value: minimax::Evaluation::Score(0),
            depth: 0,
            flag: EntryFlag::Exact,
            best_move: M::default(),
        }
    }
}

struct TranspositionTable<M> {
    table: Vec<Entry<M>>,
    mask: usize,
    minimum_depth: u8,
}

impl<M: Default> TranspositionTable<M> {
    fn new(table_size: usize) -> Self {
        let size = table_size.next_power_of_two();
        let mut table = Vec::with_capacity(size);
        for _ in 0..size {
            table.push(Entry::<M>::default());
        }
        Self { table: table, mask: size - 1, minimum_depth: 1 }
    }

    fn lookup(&self, hash: u64) -> Option<&Entry<M>> {
        let index = (hash as usize) & self.mask;
        let entry = &self.table[index];
        if hash == entry.hash {
            Some(entry)
        } else {
            None
        }
    }

    fn store(
        &mut self, hash: u64, value: minimax::Evaluation, depth: u8, flag: EntryFlag, best_move: M,
    ) {
        let index = (hash as usize) & self.mask;
        self.table[index] =
            Entry { hash: hash, value: value, depth: depth, flag: flag, best_move: best_move }
    }
}

/// Options to use for the iterative search engine.
pub struct IterativeOptions {
    max_depth: usize,
    max_time: Duration,
    table_size: usize,
}

impl Default for IterativeOptions {
    fn default() -> Self {
        IterativeOptions { max_depth: 2, max_time: Duration::default(), table_size: 1000 }
    }
}

impl IterativeOptions {
    pub fn with_timeout(mut self, dur: Duration) -> Self {
        self.max_time = dur;
        self
    }

    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    pub fn with_table_size(mut self, size: usize) -> Self {
        self.table_size = size;
        self
    }
}

pub struct IterativeSearch<E: Evaluator> {
    // These are public so that they can be changed for each move, while
    // reusing the table state between runs.
    pub max_depth: usize,
    pub max_time: Duration,
    timeout: Arc<AtomicBool>,
    transposition_table: TranspositionTable<<<E as Evaluator>::G as Game>::M>,
    _eval: PhantomData<E>,
    // Runtime stats
    // Maximum depth used to produce the move.
    actual_depth: u8,
    // Nodes explored up to this depth.
    nodes_explored: usize,
    // Nodes explored past this depth, and thus this is thrown away work.
    next_depth_nodes: usize,
    table_hits: usize,
    wall_time: Duration,
}

impl<E: Evaluator> IterativeSearch<E>
where
    <<E as Evaluator>::G as Game>::M: Default,
{
    pub fn new(opts: IterativeOptions) -> IterativeSearch<E> {
        let table = TranspositionTable::new(opts.table_size);
        IterativeSearch {
            max_depth: opts.max_depth,
            max_time: opts.max_time,
            timeout: Arc::new(AtomicBool::new(false)),
            transposition_table: table,
            _eval: PhantomData,
            actual_depth: 0,
            nodes_explored: 0,
            next_depth_nodes: 0,
            table_hits: 0,
            wall_time: Duration::default(),
        }
    }

    pub fn stats(&self) -> String {
        let throughput =
            (self.nodes_explored + self.next_depth_nodes) as f64 / self.wall_time.as_secs_f64();
        format!("Depth {} exploring {} nodes.\nPartial exploration of next depth explored {} nodes.\n{} transposition table hits.\n{:.02} nodes/sec",
		self.actual_depth, self.nodes_explored, self.next_depth_nodes, self.table_hits, throughput)
    }

    // Recursively compute negamax on the game state. Returns None if it hits the timeout.
    fn negamax(
        &mut self, s: &mut <E::G as Game>::S, depth: u8, mut alpha: Evaluation,
        mut beta: Evaluation,
    ) -> Option<Evaluation>
    where
        <E::G as Game>::S: Zobrist,
        <E::G as Game>::M: Copy + Eq,
    {
        if self.timeout.load(Ordering::Relaxed) {
            return None;
        }

        self.next_depth_nodes += 1;

        if let Some(winner) = E::G::get_winner(s) {
            return Some(winner.evaluate());
        }
        if depth == 0 {
            return Some(E::evaluate(s));
        }

        let alpha_orig = alpha;
        let hash = s.zobrist_hash();
        // TODO: encapsulate in table func
        let mut good_move = None;
        if depth >= self.transposition_table.minimum_depth {
            if let Some(entry) = self.transposition_table.lookup(hash) {
                good_move = Some(entry.best_move);
                self.table_hits += 1;
                if entry.depth >= depth {
                    match entry.flag {
                        EntryFlag::Exact => {
                            return Some(entry.value);
                        }
                        EntryFlag::Lowerbound => {
                            alpha = max(alpha, entry.value);
                        }
                        EntryFlag::Upperbound => {
                            beta = min(beta, entry.value);
                        }
                    }
                    if alpha >= beta {
                        return Some(entry.value);
                    }
                }
            }
        }

        let mut moves = [None; 200];
        let n = E::G::generate_moves(s, &mut moves);
        // Rearrange so predicted good move is first.
        for i in 0..n {
            if moves[i] == good_move {
                moves.swap(0, i);
                break;
            }
        }

        let mut best = Evaluation::Worst;
        let mut best_move = moves[0].unwrap();
        for m in moves.iter().take_while(|om| om.is_some()).map(|om| om.unwrap()) {
            m.apply(s);
            let value = -self.negamax(s, depth - 1, -beta, -alpha)?;
            m.undo(s);
            if value > best {
                best = value;
                best_move = m;
            }
            alpha = max(alpha, value);
            if alpha >= beta {
                break;
            }
        }

        // TODO: encapsulate in table func
        if depth >= self.transposition_table.minimum_depth {
            let flag = if best <= alpha_orig {
                EntryFlag::Upperbound
            } else if best >= beta {
                // TODO: beta_orig? it can change from previous entry
                EntryFlag::Lowerbound
            } else {
                EntryFlag::Exact
            };
            self.transposition_table.store(hash, best, depth, flag, best_move);
        }

        Some(best)
    }
}

impl<E: Evaluator> Strategy<E::G> for IterativeSearch<E>
where
    <E::G as Game>::S: Clone + Zobrist,
    <E::G as Game>::M: Copy + Default + Eq,
{
    fn choose_move(&mut self, s: &<E::G as Game>::S) -> Option<<E::G as Game>::M> {
        // Reset stats.
        self.nodes_explored = 0;
        self.next_depth_nodes = 0;
        self.actual_depth = 0;
        self.table_hits = 0;
        let start_time = Instant::now();
        // Start timer if configured.
        self.timeout = if self.max_time == Duration::new(0, 0) {
            Arc::new(AtomicBool::new(false))
        } else {
            timeout_signal(self.max_time)
        };

        let root_hash = s.zobrist_hash();
        let mut s_clone = s.clone();
        let mut best_move = None;

        for depth in 0..self.max_depth as u8 {
            if self.negamax(&mut s_clone, depth + 1, Evaluation::Worst, Evaluation::Best).is_none()
            {
                // Timeout. Return the best move from the previous depth.
                break;
            }
            let entry = self.transposition_table.lookup(root_hash).unwrap();
            best_move = Some(entry.best_move);

            self.actual_depth = max(self.actual_depth, depth);
            self.nodes_explored += self.next_depth_nodes;
            self.next_depth_nodes = 0;
        }
        self.wall_time = start_time.elapsed();
        best_move
    }
}
