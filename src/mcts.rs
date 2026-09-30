use crate::{GameState, Rules, Turn};
use minimax::Game;
use rand::rngs::SmallRng;
use rand::Rng;

pub struct BiasedRollouts {}

impl minimax::RolloutPolicy for BiasedRollouts {
    type G = Rules;
    fn random_move(
        &self, game: &mut GameState, turns: &mut Vec<Turn>, rng: &mut SmallRng,
    ) -> Result<Turn, minimax::Winner> {
        // TODO: lazily generate moves
        if let Some(winner) = Rules::generate_moves(game, turns) {
            return Err(winner);
        }
        let n = turns.len();
        turns.rotate_left(rng.random_range(0..n));
        for &turn in turns.iter() {
            game.apply(turn);
            if game.get_winner() == Some(minimax::Winner::PlayerJustMoved) {
                game.undo(turn);
                return Ok(turn);
            }

            game.undo(turn);
        }
        Ok(turns[0])
    }
}
