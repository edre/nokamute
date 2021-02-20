extern crate rand;

use crate::uhp_client::UhpClient;
use crate::Board;
use minimax::{Game, Move};
use rand::Rng;
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
    for depth in 2.. {
        let start = Instant::now();
        let mut b = Board::new_from_game_type(game_type).unwrap();
        let count = perft_recurse(&mut b, depth);
        let dur = start.elapsed();
        let rate = count as f64 / dur.as_secs_f64();
        println!("{}\t{}\t{:?}\t{}kn/s", depth, count * 6, dur, rate as usize / 1000);
    }
}

pub fn perft_debug(engine_cmd: &[String], game_type: &str, depth: usize) {
    let mut engine = UhpClient::new(engine_cmd).unwrap();
    engine.new_game(game_type).unwrap();
    let mut board = Board::new_from_game_type(game_type).unwrap();
    // Generate random positions at the given depth, and compare output.
    let mut rng = rand::thread_rng();
    let mut moves = [None; 200];
    for iter in 0.. {
        if iter % 10000 == 0 {
            println!("iter {}", iter);
        }
        let mut stack = Vec::new();
        for _ in 0..depth {
            let n = crate::Game::generate_moves(&board, &mut moves);
            let m = moves[rng.gen_range(0, n)].unwrap();
            m.apply(&mut board);
            engine.apply(m).unwrap();
            stack.push(m);
        }
        // Check for discrepancies.
        let n = crate::Game::generate_moves(&board, &mut moves);
        let engine_moves = engine.generate_moves().unwrap();
        if n != engine_moves.len() {
            println!("game log: {}", engine.game_log());
            println!("engine moves: {}", engine.raw_generate_moves().unwrap());
            dump_difference(&mut board, iter, &moves[..n], &engine_moves);
            break;
        }
        // Unwrap
        while let Some(m) = stack.pop() {
            m.undo(&mut board);
            engine.undo().unwrap();
        }
    }
}

fn dump_difference(
    board: &mut Board, iter: u64, nokamute_moves: &[Option<crate::Move>],
    engine_moves: &[crate::Move],
) {
    println!(
        "iteration {} found discrepancy: {} vs {} moves",
        iter,
        nokamute_moves.len(),
        engine_moves.len()
    );
    println!("position:\n{}", board);
    let mut common = Vec::new();
    let mut nokamute_only = Vec::new();
    let mut engine_only = Vec::new();
    for m in nokamute_moves.iter().map(|x| x.unwrap()) {
        if engine_moves.contains(&m) {
            common.push(m);
        } else {
            nokamute_only.push(m);
        }
    }
    for &m in engine_moves.iter() {
        if !nokamute_moves.contains(&Some(m)) {
            engine_only.push(m);
        }
    }

    println!("nokamute only moves:");
    for m in nokamute_only.iter() {
        m.apply(board);
        println!("{}", board);
        m.undo(board);
    }
    println!("UHP engine only moves:");
    for m in engine_only.iter() {
        m.apply(board);
        println!("{}", board);
        m.undo(board);
    }
    println!("common moves:");
    for m in common.iter() {
        m.apply(board);
        println!("{}", board);
        m.undo(board);
    }
}
