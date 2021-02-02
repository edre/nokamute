extern crate minimax;
extern crate rand;

use minimax::*;
use rand::Rng;
use std::cmp::max;
use std::marker::PhantomData;

fn negamax<E: Evaluator>(
    s: &mut <E::G as Game>::S, depth: usize, mut alpha: Evaluation, beta: Evaluation, p: Player,
) -> Evaluation
where
    <<E as Evaluator>::G as Game>::M: Copy,
{
    let maybe_winner = E::G::get_winner(s);
    if depth == 0 || maybe_winner.is_some() {
        return p * E::evaluate(s, maybe_winner);
    }
    let mut moves = [None; 200];
    E::G::generate_moves(s, p, &mut moves);
    let mut best = Evaluation::Worst;
    for m in moves.iter().take_while(|om| om.is_some()).map(|om| om.unwrap()) {
        m.apply(s);
        let value = -negamax::<E>(s, depth - 1, -beta, -alpha, -p);
        m.undo(s);
        best = max(best, value);
        alpha = max(alpha, value);
        if alpha >= beta {
            break;
        }
    }
    best
}

/// Options to use for the Iterative engine.
pub struct NegamaxOptions {
    pub max_depth: usize,
}

pub struct Negamax<E> {
    opts: NegamaxOptions,
    rng: rand::ThreadRng,
    _eval: PhantomData<E>,
}

impl<E: Evaluator> Negamax<E> {
    pub fn new(opts: NegamaxOptions) -> Negamax<E> {
        Negamax { opts: opts, rng: rand::thread_rng(), _eval: PhantomData }
    }
}

impl<E: Evaluator> Strategy<E::G> for Negamax<E>
where
    <E::G as Game>::S: Clone,
    <E::G as Game>::M: Copy,
{
    fn choose_move(&mut self, s: &<E::G as Game>::S, p: Player) -> Option<<E::G as Game>::M> {
        let mut best = Evaluation::Worst;
        let mut moves = [None; 200];
        E::G::generate_moves(s, p, &mut moves);
        let mut candidate_moves = Vec::new();
        let mut s_clone = s.clone();
        for m in moves.iter().take_while(|m| m.is_some()).map(|m| m.unwrap()) {
            // determine value for this move
            m.apply(&mut s_clone);
            let value = -negamax::<E>(
                &mut s_clone,
                self.opts.max_depth,
                Evaluation::Worst,
                Evaluation::Best,
                -p,
            );
            m.undo(&mut s_clone);
            // this move is a candidate move
            if value == best {
                candidate_moves.push(m);
            // this move is better than any previous, so it's the sole candidate
            } else if value > best {
                candidate_moves.clear();
                candidate_moves.push(m);
                best = value;
            }
        }
        if candidate_moves.is_empty() {
            None
        } else {
            Some(candidate_moves[self.rng.gen_range(0, candidate_moves.len())])
        }
    }
}
