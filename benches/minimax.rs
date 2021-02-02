#[macro_use]
extern crate bencher;
extern crate hive;
extern crate minimax;

use bencher::Bencher;
use hive::{Board, Bug, Negamax, NegamaxOptions};
use minimax::{Move, Strategy};

fn empty_board_depth4(b: &mut Bencher) {
    b.iter(|| {
        let mut board = Board::default();
        let mut strategy = Negamax::<hive::DumbEvaluator>::new(NegamaxOptions { max_depth: 4 });
        let m = strategy.choose_move(&mut board, minimax::Player::Opponent);
        assert!(m.is_some());
    });
}

fn full_board_depth2(b: &mut Bencher) {
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
        let mut strategy = Negamax::<hive::DumbEvaluator>::new(NegamaxOptions { max_depth: 2 });
        let m = strategy.choose_move(&mut board, minimax::Player::Opponent);
        assert!(m.is_some());
    });
}

benchmark_group!(benchmarks, empty_board_depth4, full_board_depth2);
benchmark_main!(benchmarks);
