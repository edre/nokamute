extern crate minimax;

use minimax::*;
use rand;
use rand::Rng;
use std::cmp::max;
use std::marker::PhantomData;

fn negamax<E: Evaluator>(
    s: &mut <E::G as Game>::S, depth: usize, mut alpha: Evaluation, beta: Evaluation,
) -> Evaluation
where
    <<E as Evaluator>::G as Game>::M: Copy,
{
    if let Some(winner) = E::G::get_winner(s) {
        return winner.evaluate();
    }
    if depth == 0 {
        return E::evaluate(s);
    }
    let mut moves = [None; 200];
    E::G::generate_moves(s, &mut moves);
    let mut best = Evaluation::Worst;
    for m in moves.iter().take_while(|om| om.is_some()).map(|om| om.unwrap()) {
        m.apply(s);
        let value = -negamax::<E>(s, depth - 1, -beta, -alpha);
        m.undo(s);
        best = max(best, value);
        alpha = max(alpha, value);
        if alpha >= beta {
            break;
        }
    }
    best
}

/// Options to use for the iterative search engine.
pub struct IterativeOptions {
    /// The maximum depth within the game tree.
    pub max_depth: usize,
}

pub struct IterativeSearch<E> {
    opts: IterativeOptions,
    rng: rand::ThreadRng,
    _eval: PhantomData<E>,
}

impl<E: Evaluator> IterativeSearch<E> {
    pub fn new(opts: IterativeOptions) -> IterativeSearch<E> {
        IterativeSearch { opts: opts, rng: rand::thread_rng(), _eval: PhantomData }
    }
}

impl<E: Evaluator> Strategy<E::G> for IterativeSearch<E>
where
    <E::G as Game>::S: Clone,
    <E::G as Game>::M: Copy,
{
    fn choose_move(&mut self, s: &<E::G as Game>::S) -> Option<<E::G as Game>::M> {
        let mut moves = [None; 200];
        let n = E::G::generate_moves(s, &mut moves);

        // Initial sort is random.
        self.rng.shuffle(&mut moves[..n]);

        for depth in 0..=self.opts.max_depth {
            let mut best = Evaluation::Worst;
            let mut s_clone = s.clone();
            let mut tagged_moves = Vec::with_capacity(n);
            for m in moves[..n].iter().map(|m| m.unwrap()) {
                // determine value for this move
                m.apply(&mut s_clone);
                let value = -negamax::<E>(&mut s_clone, depth, Evaluation::Worst, -best);
                m.undo(&mut s_clone);
                tagged_moves.push((value, m));
                best = max(best, value);
            }

            // Resort moves based on new score.
            // TODO: Sort slices in parallel to avoid allocation.
            tagged_moves.sort_by_key(|(value, _)| *value);
            // Highest value first.
            tagged_moves.reverse();
            // Reorder master list.
            for (i, (_, m)) in (0..).zip(tagged_moves.iter()) {
                moves[i] = Some(*m);
            }
        }
        moves[0]
    }
}
