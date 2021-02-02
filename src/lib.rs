mod board;
pub use board::{Board, Bug, DumbEvaluator, Loc, Move};
mod strategies;
pub use strategies::*;
mod zobrist;
