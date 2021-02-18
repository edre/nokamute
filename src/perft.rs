use crate::Board;
use minimax::{Game, Move};
use std::time::Instant;

fn perft_recurse(b: &mut Board, depth: usize) -> u64 {
    let mut moves = [None; 200];
    let n = crate::Game::generate_moves(b, &mut moves);
    if depth <= 1 {
        return n as u64;
    }
    let mut count = 0;
    for m in moves[..n].iter().map(|x| x.unwrap()) {
        m.apply(b);
        count += perft_recurse(b, depth - 1);
        m.undo(b);
    }
    count
}

pub fn perft(game_type: &str) {
    println!("{}", game_type);
    println!("depth\tcount\ttime\tkn/s");
    for depth in 1.. {
        let start = Instant::now();
        let mut b = Board::new_from_game_type(game_type).unwrap();
        let count = perft_recurse(&mut b, depth);
        let dur = start.elapsed();
        let rate = count as f64 / dur.as_secs_f64();
        println!("{}\t{}\t{:?}\t{}kn/s", depth, count * 6, dur, rate as usize / 1000);
    }
}
