#[macro_use]
extern crate bencher;
extern crate hive;
extern crate minimax;

use bencher::Bencher;
use hive::{Board, Bug, IterativeOptions, IterativeSearch};
use minimax::{Game, Move, Strategy};

fn empty_board_depth3(b: &mut Bencher) {
    b.iter(|| {
        let mut board = Board::default();
        let mut strategy =
            IterativeSearch::<hive::BasicEvaluator>::new(IterativeOptions { max_depth: 3 });
        let m = strategy.choose_move(&mut board);
        assert!(m.is_some());
    });
}

fn full_board_depth1(b: &mut Bencher) {
    b.iter(|| {
        let mut board = Board::default();
        // From some game I found online, subbed out some expansion pieces.
        hive::Move::Place(board.id((4, 0)), Bug::Queen).apply(&mut board);
        hive::Move::Place(board.id((1, 1)), Bug::Ant).apply(&mut board);
        hive::Move::Place(board.id((5, 0)), Bug::Ant).apply(&mut board);
        hive::Move::Place(board.id((3, 1)), Bug::Spider).apply(&mut board); //Ladybug
        hive::Move::Place(board.id((0, 1)), Bug::Ant).apply(&mut board);
        hive::Move::Place(board.id((6, 1)), Bug::Beetle).apply(&mut board); //Mosquito
        hive::Move::Place(board.id((4, 1)), Bug::Spider).apply(&mut board); //Pillbug
        hive::Move::Place(board.id((7, 1)), Bug::Beetle).apply(&mut board);
        hive::Move::Place(board.id((0, 2)), Bug::Spider).apply(&mut board);
        hive::Move::Place(board.id((-1, 2)), Bug::Ant).apply(&mut board);
        hive::Move::Place(board.id((4, 3)), Bug::Spider).apply(&mut board);
        hive::Move::Place(board.id((2, 2)), Bug::Spider).apply(&mut board); //Pillbug
        hive::Move::Place(board.id((4, 4)), Bug::Beetle).apply(&mut board);
        hive::Move::Place(board.id((3, 2)), Bug::Queen).apply(&mut board);
        hive::Move::Place(board.id((3, 2)), Bug::Beetle).apply(&mut board);
        hive::Move::Place(board.id((0, 3)), Bug::Spider).apply(&mut board);
        hive::Move::Pass.apply(&mut board);
        hive::Move::Place(board.id((5, 5)), Bug::Ant).apply(&mut board);
        hive::Move::Pass.apply(&mut board);
        let mut strategy =
            IterativeSearch::<hive::BasicEvaluator>::new(IterativeOptions { max_depth: 1 });
        let m = strategy.choose_move(&mut board);
        assert!(m.is_some());
    });
}

fn random_walk(b: &mut Bencher) {
    b.iter(|| {
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
    })
}

benchmark_group!(benchmarks, empty_board_depth3, full_board_depth1, random_walk);
benchmark_main!(benchmarks);
