#[macro_use]
extern crate bencher;
extern crate hive;
extern crate minimax;

use bencher::Bencher;
use minimax::strategies::negamax::{Negamax, Options};
use minimax::Strategy;
use hive::Board;

fn empty_board_depth4(b: &mut Bencher) {
    b.iter(|| {
        let mut board = Board::default();
        let mut strategy = Negamax::<hive::DumbEvaluator>::new(Options { max_depth: 4 });
        let m = strategy.choose_move(&mut board, minimax::Player::Opponent);
        assert!(m.is_some());
    });
}

/*
fn full_board_depth3(b: &mut Bencher) {
    b.iter(|| {
        let mut board = Board::default();
	// From some game I found online, subbed out some expansion pieces.
	hive::Move::Place((4,0), Bug::Queen).apply(&mut board);
	hive::Move::Place((1,1), Bug::Ant).apply(&mut board);
	hive::Move::Place((5,0), Bug::Ant).apply(&mut board);
	hive::Move::Place((3,1), Bug::Spider).apply(&mut board);//Ladybug
	hive::Move::Place((0,1), Bug::Ant).apply(&mut board);
	hive::Move::Place((6,1), Bug::Beetle).apply(&mut board);//Mosquito
	hive::Move::Place((4,1), Bug::Spider).apply(&mut board);//Pillbug
	hive::Move::Place((7,1), Bug::Beetle).apply(&mut board);
	hive::Move::Place((0,2), Bug::Spider).apply(&mut board);
	hive::Move::Place((-1,2), Bug::Ant).apply(&mut board);
	hive::Move::Place((4,3), Bug::Spider).apply(&mut board);
	hive::Move::Place((2,2), Bug::Spider).apply(&mut board);//Pillbug
	hive::Move::Place((4,4), Bug::Beetle).apply(&mut board);
	hive::Move::Place((3,2), Bug::Queen).apply(&mut board);
	hive::Move::Place((3,2), Bug::Beetle).apply(&mut board);
	hive::Move::Place((0,3), Bug::Spider).apply(&mut board);
	hive::Move::Pass.apply(&mut board);
	hive::Move::Place((5,5), Bug::Ant).apply(&mut board);
	hive::Move::Pass.apply(&mut board);
        let mut strategy = Negamax::<hive::DumbEvaluator>::new(Options { max_depth: 3 });
        let m = strategy.choose_move(&mut board, minimax::Player::Opponent);
        assert!(m.is_some());
    });
}
 */

benchmark_group!(benchmarks, empty_board_depth4);
benchmark_main!(benchmarks);
