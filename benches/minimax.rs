extern crate easybench;
extern crate minimax;
extern crate nokamute;

use minimax::{IterativeOptions, IterativeSearch, Move, Strategy};
use nokamute::{Board, Bug};

fn empty_board_depth(depth: usize) {
    let mut board = Board::default();
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::new(nokamute::BasicEvaluator::default(), options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn full_board_depth(depth: usize) {
    let mut board = Board::default();
    // From some game I found online, subbed out some expansion pieces.
    nokamute::Move::Place((4, 0), Bug::Queen).apply(&mut board);
    nokamute::Move::Place((1, 1), Bug::Ant).apply(&mut board);
    nokamute::Move::Place((5, 0), Bug::Ant).apply(&mut board);
    nokamute::Move::Place((3, 1), Bug::Ladybug).apply(&mut board);
    nokamute::Move::Place((0, 1), Bug::Ant).apply(&mut board);
    nokamute::Move::Place((6, 1), Bug::Mosquito).apply(&mut board);
    nokamute::Move::Place((4, 1), Bug::Pillbug).apply(&mut board);
    nokamute::Move::Place((7, 1), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place((0, 2), Bug::Spider).apply(&mut board);
    nokamute::Move::Place((-1, 2), Bug::Ant).apply(&mut board);
    nokamute::Move::Place((4, 3), Bug::Spider).apply(&mut board);
    nokamute::Move::Place((2, 2), Bug::Pillbug).apply(&mut board);
    nokamute::Move::Place((4, 4), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place((3, 2), Bug::Queen).apply(&mut board);
    nokamute::Move::Place((3, 2), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place((0, 3), Bug::Spider).apply(&mut board);
    nokamute::Move::Pass.apply(&mut board);
    nokamute::Move::Place((5, 5), Bug::Ant).apply(&mut board);
    nokamute::Move::Pass.apply(&mut board);
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::new(nokamute::BasicEvaluator::default(), options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn main() {
    // TODO: cmd line selection, for perf record
    println!("empty board 5: {}", easybench::bench(|| empty_board_depth(5)));
    println!("full board 3:  {}", easybench::bench(|| full_board_depth(2)));
}
