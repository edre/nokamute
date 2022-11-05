extern crate easybench;
extern crate minimax;
extern crate nokamute;

use minimax::{
    Game, IterativeOptions, IterativeSearch, Move, ParallelOptions, ParallelSearch, Strategy,
};
use nokamute::{loc_to_hex, Board, Bug, Rules, Turn};

fn empty_board_depth(depth: u8) {
    let mut board = Board::default();
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::new(nokamute::BasicEvaluator::default(), options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn full_board_depth(depth: u8) {
    let mut board = Board::default();
    // From some game I found online, subbed out some expansion pieces.
    Turn::Place(loc_to_hex((4, 0)), Bug::Queen).apply(&mut board);
    Turn::Place(loc_to_hex((1, 1)), Bug::Ant).apply(&mut board);
    Turn::Place(loc_to_hex((5, 0)), Bug::Ant).apply(&mut board);
    Turn::Place(loc_to_hex((3, 1)), Bug::Ladybug).apply(&mut board);
    Turn::Place(loc_to_hex((0, 1)), Bug::Ant).apply(&mut board);
    Turn::Place(loc_to_hex((6, 1)), Bug::Mosquito).apply(&mut board);
    Turn::Place(loc_to_hex((4, 1)), Bug::Pillbug).apply(&mut board);
    Turn::Place(loc_to_hex((7, 1)), Bug::Beetle).apply(&mut board);
    Turn::Place(loc_to_hex((0, 2)), Bug::Spider).apply(&mut board);
    Turn::Place(loc_to_hex((-1, 2)), Bug::Ant).apply(&mut board);
    Turn::Place(loc_to_hex((4, 3)), Bug::Spider).apply(&mut board);
    Turn::Place(loc_to_hex((2, 2)), Bug::Pillbug).apply(&mut board);
    Turn::Place(loc_to_hex((4, 4)), Bug::Beetle).apply(&mut board);
    Turn::Place(loc_to_hex((3, 2)), Bug::Queen).apply(&mut board);
    Turn::Place(loc_to_hex((3, 2)), Bug::Beetle).apply(&mut board);
    Turn::Place(loc_to_hex((0, 3)), Bug::Spider).apply(&mut board);
    Turn::Pass.apply(&mut board);
    Turn::Place(loc_to_hex((5, 5)), Bug::Ant).apply(&mut board);
    Turn::Pass.apply(&mut board);
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::new(nokamute::BasicEvaluator::default(), options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut board);
    assert!(m.is_some());
}

fn playout(mut depth: usize) -> Board {
    let mut board = Board::default();
    let mut rand = minimax::Random::<Rules>::new();
    while depth > 0 && Rules::get_winner(&board).is_none() {
        depth -= 1;
        let m = rand.choose_move(&board).unwrap();
        m.apply(&mut board);
    }
    board
}

// Find one random position and run the iterative strategies at a deeper level to compare timings.
fn deep_iterations() {
    let board = playout(20);
    let opts = IterativeOptions::new().verbose().with_table_byte_size(32 << 20);
    let eval = nokamute::BasicEvaluator::default();
    let strategies: [Box<dyn minimax::Strategy<nokamute::Rules>>; 2] = [
        Box::new(IterativeSearch::new(eval.clone(), opts)),
        Box::new(ParallelSearch::new(eval.clone(), opts, ParallelOptions::new())),
    ];
    for mut strategy in strategies {
        strategy.choose_move(&board);
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

    if "deep iterations".contains(&filter) {
        println!("deep iterations:");
        deep_iterations();
    }
}
