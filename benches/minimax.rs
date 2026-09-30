extern crate easybench;
extern crate minimax;
extern crate nokamute;

use minimax::{IterativeOptions, IterativeSearch, ParallelOptions, ParallelSearch, Strategy};
use nokamute::{loc_to_hex, Bug, GameState, Rules, Turn};

fn empty_game_depth(depth: u8) {
    let mut game = GameState::default();
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::new(nokamute::BasicEvaluator::default(), options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut game);
    assert!(m.is_some());
}

fn full_game_depth(depth: u8) {
    let mut game = GameState::default();
    // From some game I found online, subbed out some expansion pieces.
    game.apply(Turn::Place(loc_to_hex((4, 0)), Bug::Queen));
    game.apply(Turn::Place(loc_to_hex((1, 1)), Bug::Ant));
    game.apply(Turn::Place(loc_to_hex((5, 0)), Bug::Ant));
    game.apply(Turn::Place(loc_to_hex((3, 1)), Bug::Ladybug));
    game.apply(Turn::Place(loc_to_hex((0, 1)), Bug::Ant));
    game.apply(Turn::Place(loc_to_hex((6, 1)), Bug::Mosquito));
    game.apply(Turn::Place(loc_to_hex((4, 1)), Bug::Pillbug));
    game.apply(Turn::Place(loc_to_hex((7, 1)), Bug::Beetle));
    game.apply(Turn::Place(loc_to_hex((0, 2)), Bug::Spider));
    game.apply(Turn::Place(loc_to_hex((-1, 2)), Bug::Ant));
    game.apply(Turn::Place(loc_to_hex((4, 3)), Bug::Spider));
    game.apply(Turn::Place(loc_to_hex((2, 2)), Bug::Pillbug));
    game.apply(Turn::Place(loc_to_hex((4, 4)), Bug::Beetle));
    game.apply(Turn::Place(loc_to_hex((3, 2)), Bug::Queen));
    game.apply(Turn::Place(loc_to_hex((3, 2)), Bug::Beetle));
    game.apply(Turn::Place(loc_to_hex((0, 3)), Bug::Spider));
    game.apply(Turn::Pass);
    game.apply(Turn::Place(loc_to_hex((5, 5)), Bug::Ant));
    game.apply(Turn::Pass);
    let options = IterativeOptions::new().with_table_byte_size(16000).with_null_window_search(true);
    let mut strategy = IterativeSearch::new(nokamute::BasicEvaluator::default(), options);
    strategy.set_max_depth(depth);
    let m = strategy.choose_move(&mut game);
    assert!(m.is_some());
}

fn playout(mut depth: usize) -> GameState {
    let mut game = GameState::default();
    let mut rand = minimax::Random::<Rules>::new();
    while depth > 0 && game.get_winner().is_none() {
        depth -= 1;
        game.apply(rand.choose_move(&game).unwrap());
    }
    game
}

// Find one random position and run the iterative strategies at a deeper level to compare timings.
fn deep_iterations() {
    let game = playout(20);
    let opts = IterativeOptions::new().verbose().with_table_byte_size(32 << 20);
    let eval = nokamute::BasicEvaluator::default();
    let strategies: [Box<dyn minimax::Strategy<nokamute::Rules>>; 2] = [
        Box::new(IterativeSearch::new(eval.clone(), opts)),
        Box::new(ParallelSearch::new(eval.clone(), opts, ParallelOptions::new())),
    ];
    for mut strategy in strategies {
        strategy.choose_move(&game);
    }
}

fn main() {
    let mut filter = std::env::args().skip(1).next().unwrap_or("".to_string());
    if filter == "--bench" {
        filter = "".to_string();
    }
    if "empty game".contains(&filter) {
        println!("empty game 5:  {}", easybench::bench(|| empty_game_depth(5)));
    }
    if "full game".contains(&filter) {
        println!("full game 3:   {}", easybench::bench(|| full_game_depth(2)));
    }
    if "random playout".contains(&filter) {
        println!("random playout: {}", easybench::bench(|| playout(200)));
    }

    if "deep iterations".contains(&filter) {
        println!("deep iterations:");
        deep_iterations();
    }
}
