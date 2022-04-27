extern crate rand;

use crate::uhp_client::UhpClient;
use crate::uhp_util::UhpBoard;
use crate::{Board, Rules};
use minimax::{Game, Move};
use rand::Rng;

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
        b.println();
    }
    minimax::perft::<Rules>(&mut b, 20, false);
}

pub fn perft_multi_thread(game_string: &str) {
    let game_string = standard_games(game_string);
    println!("{}", game_string);
    let mut b = UhpBoard::from_game_string(game_string).unwrap().into_inner();
    if game_string.contains(';') {
        b.println();
    }
    minimax::perft::<Rules>(&mut b, 20, true);
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
        if iter % 100 == 0 {
            println!("iter {}", iter);
        }
        // Roll out a random game to the desired depth.
        let mut stack = Vec::new();
        for _ in 0..depth {
            // Check for discrepancies on every move.
            moves.clear();
            Rules::generate_moves(&board, &mut moves);
            let engine_moves = engine.generate_moves().unwrap();
            if moves.len() != engine_moves.len() {
                println!("game log: {}", engine.game_log());
                dump_difference(&mut board, iter, &moves, &engine_moves);
                return;
            }

            let m = moves[rng.gen_range(0, moves.len())];
            stack.push(m);
            m.apply(&mut board);
            let board_winner = Rules::get_winner(&board);
            let engine_winner = engine.apply(m).unwrap();
            if board_winner != engine_winner {
                println!(
                    "iter {} game end disagreement: board_winner={:?} engine_winner={:?}",
                    iter, board_winner, engine_winner
                );
                println!("game log: {}", engine.game_log());
                board.println();
                return;
            }
            if board_winner.is_some() {
                break;
            }
        }

        // Unwrap
        engine.undo(stack.len()).unwrap();
        while let Some(m) = stack.pop() {
            m.undo(&mut board);
        }
    }
}

fn dump_difference(
    board: &mut Board, iter: usize, nokamute_moves: &[crate::Move], engine_moves: &[crate::Move],
) {
    println!(
        "iteration {} found discrepancy: {} vs {} moves",
        iter,
        nokamute_moves.len(),
        engine_moves.len()
    );
    println!("position:");
    board.println();
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

    let nokamute_dups = find_dups(nokamute_moves);
    let engine_dups = find_dups(engine_moves);

    let mut print_moves = |title: &str, moves: &[crate::Move]| {
        if !moves.is_empty() {
            println!("{}:", title);
        }
        for m in moves.iter() {
            m.apply(board);
            board.println();
            m.undo(board);
        }
    };

    print_moves("nokamute only moves", &nokamute_only);
    print_moves("UHP engine only moves", &engine_only);
    print_moves("nokamute duplicate moves", &nokamute_dups);
    print_moves("engine duplicate moves", &engine_dups);
}

fn find_dups(moves: &[crate::Move]) -> Vec<crate::Move> {
    let mut dups = Vec::new();
    for &m in moves.iter() {
        if moves.iter().filter(|&&m2| m == m2).count() > 1 && !dups.contains(&m) {
            dups.push(m);
        }
    }
    dups
}

#[test]
fn test_perft() {
    let mut b = Board::new_from_game_type("Base").unwrap();
    let move_counts = minimax::perft::<Rules>(&mut b, 4, false);
    assert_eq!(move_counts, vec![1, 4, 96, 1440, 21600]);

    b = Board::new_from_game_type("Base+MLP").unwrap();
    let move_counts = minimax::perft::<Rules>(&mut b, 4, false);
    assert_eq!(move_counts, vec![1, 7, 294, 6678, 151686]);
}
