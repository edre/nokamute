extern crate easybench;
extern crate minimax;
extern crate nokamute;
extern crate rand;

use minimax::{Game, IterativeOptions, IterativeSearch, Move, Strategy};
use nokamute::{loc_to_id, Board, Bug, Rules};
use rand::Rng;

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
    nokamute::Move::Place(loc_to_id((4, 0)), Bug::Queen).apply(&mut board);
    nokamute::Move::Place(loc_to_id((1, 1)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(loc_to_id((5, 0)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(loc_to_id((3, 1)), Bug::Ladybug).apply(&mut board);
    nokamute::Move::Place(loc_to_id((0, 1)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(loc_to_id((6, 1)), Bug::Mosquito).apply(&mut board);
    nokamute::Move::Place(loc_to_id((4, 1)), Bug::Pillbug).apply(&mut board);
    nokamute::Move::Place(loc_to_id((7, 1)), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place(loc_to_id((0, 2)), Bug::Spider).apply(&mut board);
    nokamute::Move::Place(loc_to_id((-1, 2)), Bug::Ant).apply(&mut board);
    nokamute::Move::Place(loc_to_id((4, 3)), Bug::Spider).apply(&mut board);
    nokamute::Move::Place(loc_to_id((2, 2)), Bug::Pillbug).apply(&mut board);
    nokamute::Move::Place(loc_to_id((4, 4)), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place(loc_to_id((3, 2)), Bug::Queen).apply(&mut board);
    nokamute::Move::Place(loc_to_id((3, 2)), Bug::Beetle).apply(&mut board);
    nokamute::Move::Place(loc_to_id((0, 3)), Bug::Spider).apply(&mut board);
    nokamute::Move::Pass.apply(&mut board);
    nokamute::Move::Place(loc_to_id((5, 5)), Bug::Ant).apply(&mut board);
    nokamute::Move::Pass.apply(&mut board);
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::new(nokamute::BasicEvaluator::default(), options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn playout(mut depth: usize) {
    let mut board = Board::default();
    let mut moves = Vec::new();
    let mut rng = rand::thread_rng();
    while depth > 0 && Rules::get_winner(&board).is_none() {
        depth -= 1;
        moves.clear();
        Rules::generate_moves(&board, &mut moves);
        let m = moves[rng.gen_range(0, moves.len())];
        m.apply(&mut board);
    }
}

fn main() {
    let mut filter = std::env::args().skip(1).next().unwrap_or("".to_string());
    if filter == "--bench" {
        filter = "".to_string();
    }
    if "empty board".contains(&filter) {
        println!("empty board 5:  {}", easybench::bench(|| empty_board_depth(5)));
    }
    if "full board".contains(&filter) {
        println!("full board 3:   {}", easybench::bench(|| full_board_depth(2)));
    }
    if "random playout".contains(&filter) {
        println!("random playout: {}", easybench::bench(|| playout(200)));
    }
}
