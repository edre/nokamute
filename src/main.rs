use hive;
use minimax::Move;

fn main() {
    let mut board = hive::Board::default();
    hive::Move::Place((0, 0), hive::Bug::Queen).apply(&mut board);
    hive::Move::Place((1, 0), hive::Bug::Spider).apply(&mut board);
    hive::Move::Place((-1, -1), hive::Bug::Beetle).apply(&mut board);
    hive::Move::Place((2, 1), hive::Bug::Ant).apply(&mut board);
    hive::Move::Place((-1, 0), hive::Bug::Grasshopper).apply(&mut board);
    println!("{}", board);
}
