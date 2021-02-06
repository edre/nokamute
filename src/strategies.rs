extern crate minimax;

use minimax::*;
use rand;
use rand::Rng;
use std::cmp::max;
use std::marker::PhantomData;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};

fn timeout_signal(dur: Duration) -> Arc<AtomicBool> {
    // Theoretically we could include an async runtime to do this and clean up
    // old threads, but the stdlib implementation is just a couple lines...
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

/// Options to use for the iterative search engine.
pub struct IterativeOptions {
    max_depth: Option<usize>,
    max_time: Option<Duration>,
}

impl IterativeOptions {
    pub fn with_timeout(dur: Duration) -> Self {
        IterativeOptions { max_depth: None, max_time: Some(dur) }
    }

    pub fn with_max_depth(depth: usize) -> Self {
        IterativeOptions { max_depth: Some(depth), max_time: None }
    }
}

pub struct IterativeSearch<E> {
    opts: IterativeOptions,
    rng: rand::ThreadRng,
    timeout: Arc<AtomicBool>,
    _eval: PhantomData<E>,
    // Runtime stats
    // Maximum depth used to produce the move.
    max_depth: usize,
    // Nodes explored up to this depth.
    nodes_explored: usize,
    // Nodes explored past this depth, and thus this is thrown away work.
    next_depth_nodes: usize,
    wall_time: Duration,
}

impl<E: Evaluator> IterativeSearch<E> {
    pub fn new(opts: IterativeOptions) -> IterativeSearch<E> {
        IterativeSearch {
            opts: opts,
            rng: rand::thread_rng(),
            timeout: Arc::new(AtomicBool::new(false)),
            _eval: PhantomData,
            max_depth: 0,
            nodes_explored: 0,
            next_depth_nodes: 0,
            wall_time: Duration::new(0, 0),
        }
    }

    pub fn stats(&self) -> String {
        let throughput =
            (self.nodes_explored + self.next_depth_nodes) as f64 / self.wall_time.as_secs_f64();
        format!("Depth {} exploring {} nodes.\nPartial exploration of next depth explored {} nodes.\n{:.02} nodes/sec",
		self.max_depth, self.nodes_explored, self.next_depth_nodes, throughput)
    }

    // Recursively compute negamax on the game state. Returns None if it hits the timeout.
    fn negamax(
        &mut self, s: &mut <E::G as Game>::S, depth: usize, mut alpha: Evaluation, beta: Evaluation,
    ) -> Option<Evaluation>
    where
        <E::G as Game>::S: Zobrist,
        <E::G as Game>::M: Copy,
    {
        s.zobrist_hash(); // Test that we plumbed the traits correctly.
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

        let mut moves = [None; 200];
        E::G::generate_moves(s, &mut moves);
        let mut best = Evaluation::Worst;
        for m in moves.iter().take_while(|om| om.is_some()).map(|om| om.unwrap()) {
            m.apply(s);
            let value = -self.negamax(s, depth - 1, -beta, -alpha)?;
            m.undo(s);
            best = max(best, value);
            alpha = max(alpha, value);
            if alpha >= beta {
                break;
            }
        }
        Some(best)
    }
}

impl<E: Evaluator> Strategy<E::G> for IterativeSearch<E>
where
    <E::G as Game>::S: Clone + Zobrist,
    <E::G as Game>::M: Copy,
{
    fn choose_move(&mut self, s: &<E::G as Game>::S) -> Option<<E::G as Game>::M> {
        // Reset stats.
        self.nodes_explored = 0;
        self.next_depth_nodes = 0;
        self.max_depth = 0;
        let start_time = Instant::now();
        // Start timer if configured.
        self.timeout = if let Some(max_time) = self.opts.max_time {
            timeout_signal(max_time)
        } else {
            Arc::new(AtomicBool::new(false))
        };

        let mut moves = [None; 200];
        let n = E::G::generate_moves(s, &mut moves);
        let mut s_clone = s.clone();

        // Initial sort is random.
        self.rng.shuffle(&mut moves[..n]);

        for depth in 0..=self.opts.max_depth.unwrap_or(50) {
            let mut best = Evaluation::Worst;
            let mut tagged_moves = Vec::with_capacity(n);
            for m in moves[..n].iter().map(|m| m.unwrap()) {
                // determine value for this move
                m.apply(&mut s_clone);
                let value = if let Some(ret) =
                    self.negamax(&mut s_clone, depth, Evaluation::Worst, -best)
                {
                    -ret
                } else {
                    // Timeout. Return the best move from the previous depth.
                    break;
                };
                m.undo(&mut s_clone);

                tagged_moves.push((value, m));
                best = max(best, value);
            }
            if self.timeout.load(Ordering::Relaxed) {
                break;
            }

            // Resort moves based on new score.
            // TODO: Sort slices in parallel to avoid allocation?
            tagged_moves.sort_by_key(|(value, _)| *value);
            // Highest value first.
            tagged_moves.reverse();
            // Reorder master list.
            for (i, (_, m)) in (0..).zip(tagged_moves.iter()) {
                moves[i] = Some(*m);
            }

            self.max_depth = max(self.max_depth, depth);
            self.nodes_explored += self.next_depth_nodes;
            self.next_depth_nodes = 0;
        }
        self.wall_time = Instant::now() - start_time;
        moves[0]
    }
}
