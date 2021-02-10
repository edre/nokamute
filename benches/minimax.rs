extern crate easybench;
extern crate hive;
extern crate minimax;

use hive::{Board, Bug, IterativeOptions, IterativeSearch};
use minimax::{Game, Move, Strategy};

fn empty_board_depth(depth: usize) {
    let mut board = Board::default();
    let options = IterativeOptions::default().with_max_depth(depth).with_table_size(2000);
    let mut strategy = IterativeSearch::<hive::BasicEvaluator>::new(options);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn full_board_depth(depth: usize) {
    let mut board = Board::default();
    // From some game I found online, subbed out some expansion pieces.
    hive::Move::Place(board.id((4, 0)), Bug::Queen).apply(&mut board);
    hive::Move::Place(board.id((1, 1)), Bug::Ant).apply(&mut board);
    hive::Move::Place(board.id((5, 0)), Bug::Ant).apply(&mut board);
    hive::Move::Place(board.id((3, 1)), Bug::Ladybug).apply(&mut board);
    hive::Move::Place(board.id((0, 1)), Bug::Ant).apply(&mut board);
    hive::Move::Place(board.id((6, 1)), Bug::Mosquito).apply(&mut board);
    hive::Move::Place(board.id((4, 1)), Bug::Pillbug).apply(&mut board);
    hive::Move::Place(board.id((7, 1)), Bug::Beetle).apply(&mut board);
    hive::Move::Place(board.id((0, 2)), Bug::Spider).apply(&mut board);
    hive::Move::Place(board.id((-1, 2)), Bug::Ant).apply(&mut board);
    hive::Move::Place(board.id((4, 3)), Bug::Spider).apply(&mut board);
    hive::Move::Place(board.id((2, 2)), Bug::Pillbug).apply(&mut board);
    hive::Move::Place(board.id((4, 4)), Bug::Beetle).apply(&mut board);
    hive::Move::Place(board.id((3, 2)), Bug::Queen).apply(&mut board);
    hive::Move::Place(board.id((3, 2)), Bug::Beetle).apply(&mut board);
    hive::Move::Place(board.id((0, 3)), Bug::Spider).apply(&mut board);
    hive::Move::Pass.apply(&mut board);
    hive::Move::Place(board.id((5, 5)), Bug::Ant).apply(&mut board);
    hive::Move::Pass.apply(&mut board);
    let options = IterativeOptions::default().with_max_depth(depth).with_table_size(200);
    let mut strategy = IterativeSearch::<hive::BasicEvaluator>::new(options);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn random_walk() {
    // Simple deterministic RNG.
    let mut rand = 12345u32;
    let mut board = Board::default();
    for _ in 0..300 {
        if hive::Game::get_winner(&board).is_some() {
            break;
        }
        let mut moves = [None; 200];
        let n = hive::Game::generate_moves(&board, &mut moves);
        // Iterate RNG
        rand = rand.wrapping_mul(101).wrapping_add(1);
        let m = moves[rand as usize % n].unwrap();
        m.apply(&mut board);
    }
}

fn main() {
    // TODO: cmd line selection, for perf record
    println!("empty board 5: {}", easybench::bench(|| empty_board_depth(5)));
    println!("full board 3:  {}", easybench::bench(|| full_board_depth(2)));
    println!("random walk:   {}", easybench::bench(random_walk));
}
