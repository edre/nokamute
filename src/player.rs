extern crate minimax;

use crate::cli::CliPlayer;
use crate::uhp_client::UhpPlayer;
use crate::{BasicEvaluator, Board, Rules};
use minimax::{Game, IterativeOptions, IterativeSearch, Move, Strategy};
use rand::Rng;
use std::time::Duration;

// A player that can play one color's moves.
pub(crate) trait Player {
    fn name(&self) -> String;
    fn new_game(&mut self, game_type: &str);
    fn play_move(&mut self, m: crate::Move);
    fn generate_move(&mut self) -> crate::Move;
}

fn face_off(
    game_type: &str, mut player1: Box<dyn Player>, mut player2: Box<dyn Player>,
) -> Option<String> {
    let mut b = Board::new_from_game_type(game_type).unwrap();
    player1.new_game(game_type);
    player2.new_game(game_type);
    let mut players = [player1, player2];
    let mut p = 0;
    loop {
        b.println();
        println!("{} ({:?}) to move", players[p].name(), b.to_move());
        let m = players[p].generate_move();
        let mut moves = Vec::new();
        Rules::generate_moves(&b, &mut moves);
        if !moves.contains(&m) {
            println!("{} played an illegal move.", players[p].name());
            return Some(players[1 - p].name());
        }
        m.apply(&mut b);
        if let Some(winner) = Rules::get_winner(&b) {
            b.println();
            return match winner {
                minimax::Winner::Draw => None,
                minimax::Winner::PlayerJustMoved => Some(players[p].name()),
                minimax::Winner::PlayerToMove => Some(players[1 - p].name()),
            };
        }
        players[p].play_move(m);
        p = 1 - p;
        players[p].play_move(m);
    }
}

fn get_player(name: &str) -> Box<dyn Player> {
    match name {
        "nokamute" => Box::new(NokamutePlayer::new()),
        "ai" => Box::new(NokamutePlayer::new()),
        "human" => Box::new(CliPlayer::new()),
        "random" => Box::new(RandomPlayer::default()),
        // Try to launch this as a UHP server
        _ => Box::new(UhpPlayer::new(name).unwrap()),
    }
}

pub fn play_game(game_type: &str, name1: &str, name2: &str) {
    let player1 = get_player(name1);
    let player2 = get_player(name2);
    match face_off(game_type, player1, player2) {
        None => println!("Game over: draw."),
        Some(name) => println!("Game over: {} won.", name),
    }
}

struct NokamutePlayer {
    board: Board,
    strategy: IterativeSearch<BasicEvaluator>,
}

impl NokamutePlayer {
    fn new() -> Self {
        let opts =
            IterativeOptions::new().with_table_byte_size(32_000_000).with_null_window_search(true);
        let mut strategy = IterativeSearch::new(BasicEvaluator::default(), opts);
        strategy.set_timeout(Duration::from_secs(5));
        NokamutePlayer { board: Board::default(), strategy }
    }
}

impl Player for NokamutePlayer {
    fn name(&self) -> String {
        "nokamute".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.board = Board::new_from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        m.apply(&mut self.board);
    }

    fn generate_move(&mut self) -> crate::Move {
        self.strategy.choose_move(&self.board).unwrap()
    }
}

#[derive(Default)]
struct RandomPlayer {
    board: Board,
}

impl Player for RandomPlayer {
    fn name(&self) -> String {
        "random".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.board = Board::new_from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        m.apply(&mut self.board);
    }

    fn generate_move(&mut self) -> crate::Move {
        let mut moves = Vec::new();
        Rules::generate_moves(&self.board, &mut moves);
        moves[rand::thread_rng().gen_range(0, moves.len())]
    }
}
