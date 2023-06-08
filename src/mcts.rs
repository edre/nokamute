use crate::{Board, Rules, Turn};
use minimax;
use minimax::Game;
use rand::rngs::ThreadRng;
use rand::Rng;

pub struct BiasedRollouts {}

impl minimax::RolloutPolicy for BiasedRollouts {
    type G = Rules;
    fn random_move(&self, board: &mut Board, turns: &mut Vec<Turn>, rng: &mut ThreadRng) -> Turn {
        // TODO: lazily generate moves
        Rules::generate_moves(board, turns);
        let n = turns.len();
        turns.rotate_left(rng.gen_range(0..n));
        for &turn in turns.iter() {
            board.apply(turn);
            if Rules::get_winner(board) == Some(minimax::Winner::PlayerJustMoved) {
                board.undo(turn);
                return turn;
            }

            board.undo(turn);
        }
        return turns[0];
    }
}
