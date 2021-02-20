extern crate easybench;
extern crate minimax;
extern crate nokamute;

use minimax::{Game, IterativeOptions, IterativeSearch, Move, Strategy};
use nokamute::{Board, Bug};

fn empty_board_depth(depth: usize) {
    let mut board = Board::default();
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::<nokamute::BasicEvaluator>::new(options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn full_board_depth(depth: usize) {
    let mut board = Board::default();
    // From some game I found online, subbed out some expansion pieces.
    nokamute::Move::Place(board.id((4, 0)), Bug::Queen).apply(&mut board);
    nokamute::Move::Place(board.id((1, 1)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(board.id((5, 0)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(board.id((3, 1)), Bug::Ladybug).apply(&mut board);
    nokamute::Move::Place(board.id((0, 1)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(board.id((6, 1)), Bug::Mosquito).apply(&mut board);
    nokamute::Move::Place(board.id((4, 1)), Bug::Pillbug).apply(&mut board);
    nokamute::Move::Place(board.id((7, 1)), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place(board.id((0, 2)), Bug::Spider).apply(&mut board);
    nokamute::Move::Place(board.id((-1, 2)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(board.id((4, 3)), Bug::Spider).apply(&mut board);
    nokamute::Move::Place(board.id((2, 2)), Bug::Pillbug).apply(&mut board);
    nokamute::Move::Place(board.id((4, 4)), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place(board.id((3, 2)), Bug::Queen).apply(&mut board);
    nokamute::Move::Place(board.id((3, 2)), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place(board.id((0, 3)), Bug::Spider).apply(&mut board);
    nokamute::Move::Pass.apply(&mut board);
    nokamute::Move::Place(board.id((5, 5)), Bug::Ant).apply(&mut board);
    nokamute::Move::Pass.apply(&mut board);
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::<nokamute::BasicEvaluator>::new(options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn random_walk() {
    // Simple deterministic RNG.
    let mut rand = 12345u32;
    let mut board = Board::default();
    for _ in 0..300 {
        if nokamute::Game::get_winner(&board).is_some() {
            break;
        }
        let mut moves = [None; 200];
        let n = nokamute::Game::generate_moves(&board, &mut moves);
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
