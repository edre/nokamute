extern crate rand;
extern crate rayon;

use crate::uhp_client::UhpClient;
use crate::uhp_util::UhpBoard;
use crate::{Board, Rules};
use minimax::{Game, Move};
use rand::Rng;
use rayon::prelude::*;
use std::time::Instant;

fn perft_recurse(b: &mut Board, depth: usize) -> u64 {
    if depth == 0 {
        return 1;
    }
    if Rules::get_winner(b).is_some() {
        // Apparently perft rules only count positions at the target depth.
        return 0;
    }
    let mut moves = Vec::new();
    Rules::generate_moves(b, &mut moves);
    if depth == 1 {
        moves.len() as u64
    } else if depth < 4 {
        // Serial exploration of leafy nodes, to avoid excessive cloning.
        let mut count = 0;
        for m in moves.iter() {
            m.apply(b);
            count += perft_recurse(b, depth - 1);
            m.undo(b);
        }
        count
    } else {
        moves
            .into_par_iter()
            .with_max_len(1)
            .map(|m| {
                let mut b2 = b.clone();
                m.apply(&mut b2);
                perft_recurse(&mut b2, depth - 1)
                // no reason to undo
            })
            .sum()
    }
}

fn standard_games(game_string: &str) -> &str {
    match game_string {
	"beetle_gate" => "Base+MLP;InProgress;White[5];wB1;bB1 -wB1;wQ wB1-;bQ /bB1;wB2 wQ-;bQ /wB1;wB2 wQ;bB1 bQ",
	_ => game_string,
    }
}

pub fn perft_single_thread(game_string: &str) {
    let game_string = standard_games(game_string);
    println!("{}", game_string);
    let mut b = UhpBoard::from_game_string(game_string).unwrap().into_inner();
    if game_string.contains(';') {
        print!("{}", b);
    }
    minimax::perft::<Rules>(&mut b, 20);
}

pub fn perft_multi_thread(game_string: &str) {
    let game_string = standard_games(game_string);
    println!("{}", game_string);
    let mut b = UhpBoard::from_game_string(game_string).unwrap().into_inner();
    println!("{}depth\tcount\ttime\tkn/s", b);
    for depth in 0.. {
        let start = Instant::now();
        let count = perft_recurse(&mut b, depth);
        let dur = start.elapsed();
        let rate = count as f64 / dur.as_secs_f64();
        println!("{}\t{}\t{:?}\t{}", depth, count, dur, rate as usize / 1000);
    }
}

pub fn perft_debug(engine_cmd: &[String], game_string: &str, depth: usize) {
    let game_string = standard_games(game_string);
    let mut engine = UhpClient::new(engine_cmd).unwrap();
    engine.new_game(game_string).unwrap();
    let mut board = UhpBoard::from_game_string(game_string).unwrap().into_inner();
    // Generate random positions at the given depth, and compare output.
    let mut rng = rand::thread_rng();
    let mut moves = Vec::new();
    for iter in 0.. {
        if iter % 10000 == 0 {
            println!("iter {}", iter);
        }
        let mut stack = Vec::new();
        for _ in 0..depth {
            Rules::generate_moves(&board, &mut moves);
            let m = moves[rng.gen_range(0, moves.len())];
            m.apply(&mut board);
            engine.apply(m).unwrap();
            stack.push(m);
        }
        // Check for discrepancies.
        Rules::generate_moves(&board, &mut moves);
        let engine_moves = engine.generate_moves().unwrap();
        if moves.len() != engine_moves.len() {
            println!("game log: {}", engine.game_log());
            println!("engine moves: {}", engine.raw_generate_moves().unwrap());
            dump_difference(&mut board, iter, &moves, &engine_moves);
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
    board: &mut Board, iter: u64, nokamute_moves: &[crate::Move], engine_moves: &[crate::Move],
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
    for &m in nokamute_moves.iter() {
        if engine_moves.contains(&m) {
            common.push(m);
        } else {
            nokamute_only.push(m);
        }
    }
    for &m in engine_moves.iter() {
        if !nokamute_moves.contains(&m) {
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

#[test]
fn test_perft() {
    let mut b = Board::new_from_game_type("Base").unwrap();
    let move_counts = minimax::perft::<Rules>(&mut b, 4);
    assert_eq!(move_counts, vec![1, 4, 96, 1440, 21600]);

    b = Board::new_from_game_type("Base+MLP").unwrap();
    let move_counts = minimax::perft::<Rules>(&mut b, 4);
    assert_eq!(move_counts, vec![1, 7, 294, 6678, 151686]);
}
