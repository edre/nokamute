extern crate hexbugs;
extern crate minimax;

use hexbugs::{GameState, Turn};

pub struct Rules;

pub(crate) fn minimax_winner(winner: hexbugs::Winner) -> minimax::Winner {
    match winner {
        hexbugs::Winner::PlayerJustMoved => minimax::Winner::PlayerJustMoved,
        hexbugs::Winner::PlayerToMove => minimax::Winner::PlayerToMove,
        hexbugs::Winner::Draw => minimax::Winner::Draw,
    }
}

impl minimax::Game for Rules {
    type S = GameState;
    type M = Turn;

    fn generate_moves(game: &GameState, turns: &mut Vec<Turn>) -> Option<minimax::Winner> {
        if let Some(winner) = game.get_winner() {
            return Some(minimax_winner(winner));
        }
        game.generate_moves(turns);
        None
    }

    fn get_winner(game: &GameState) -> Option<minimax::Winner> {
        game.get_winner().map(minimax_winner)
    }

    fn apply(game: &mut GameState, turn: Turn) -> Option<GameState> {
        game.apply(turn);
        None
    }

    fn undo(game: &mut GameState, turn: Turn) {
        game.undo(turn);
    }

    fn zobrist_hash(game: &GameState) -> u64 {
        game.zobrist_hash()
    }

    fn null_move(_: &GameState) -> Option<Turn> {
        Some(Turn::Pass)
    }

    fn notation(game: &GameState, turn: Turn) -> Option<String> {
        Some(game.to_move_string(turn))
    }

    fn table_index(turn: Turn) -> u16 {
        // Arbitarily squeezed smaller by shaving off
        // the highest bit from each hex.
        const MASK: u16 = 0x7f;
        #[allow(clippy::unnecessary_cast)]
        match turn {
            Turn::Place(hex, bug) => ((bug as u16) << 7) | hex as u16 & MASK,
            Turn::Move(start, end) => ((start as u16 & MASK) << 7) | end as u16 & MASK,
            Turn::Pass => 0,
        }
    }
    /// Maximum table size.
    fn max_table_index() -> u16 {
        u16::MAX >> 2
    }
}
