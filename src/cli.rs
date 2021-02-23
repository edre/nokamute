use crate::player::Player;
use crate::{Board, Bug, Loc, Rules};
use minimax::{Game, IterativeOptions, IterativeSearch, Move, Strategy};
use std::io::{self, BufRead, Write};
use std::time::Duration;

fn read_line(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    io::stdin().lock().lines().next().unwrap().unwrap()
}

fn input_loc(board: &Board, prompt: &str, options: &[Loc]) -> Option<Loc> {
    println!("{}", board.fancy_fmt(options));
    let line = read_line(prompt);
    let index = if let Ok(num) = line.parse::<usize>() {
        num
    } else {
        println!("Invalid. Enter a number.");
        return None;
    };
    if index >= options.len() {
        println!("Not one of the options.");
        return None;
    }
    Some(options[index])
}

fn input_bug(options: &[Bug]) -> Option<Bug> {
    for bug in Bug::iter_all() {
        if options.contains(&bug) {
            print!("{}:{}, ", bug.codepoint(), bug.name());
        }
    }
    println!("");

    let line = read_line("Which bug? ");
    let bug = Bug::from_char(line.chars().next().unwrap_or('?'));
    if bug.is_none() {
        println!("Unrecognized bug.");
    }
    bug
}

fn input_movement(board: &Board, moves: &[Option<crate::Move>]) -> Option<crate::Move> {
    let mut starts =
        moves
            .iter()
            .filter_map(|m| {
                if let Some(crate::Move::Movement(start, _)) = m {
                    Some(*start)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
    starts.sort();
    starts.dedup();
    if starts.is_empty() {
        println!("No movements available.");
        return None;
    }
    let start = input_loc(board, "Move which bug? ", &starts)?;

    let mut ends = moves
        .iter()
        .filter_map(|m| {
            if let Some(crate::Move::Movement(start2, end)) = m {
                if start == *start2 {
                    Some(*end)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    ends.sort();
    ends.dedup();
    let end = input_loc(board, "Move to where? ", &ends)?;

    Some(crate::Move::Movement(start, end))
}

fn input_placement(board: &Board, moves: &[Option<crate::Move>]) -> Option<crate::Move> {
    let mut places = moves
        .iter()
        .filter_map(
            |m| if let Some(crate::Move::Place(place, _)) = m { Some(*place) } else { None },
        )
        .collect::<Vec<_>>();
    places.sort();
    places.dedup();
    if places.is_empty() {
        println!("No placements available.");
        return None;
    }
    let place = input_loc(board, "Place new bug where? ", &places)?;

    let bugs = moves
        .iter()
        .filter_map(|m| if let Some(crate::Move::Place(_, bug)) = m { Some(*bug) } else { None })
        .collect::<Vec<_>>();
    let bug = input_bug(&bugs)?;

    Some(crate::Move::Place(place, bug))
}

pub(crate) struct CliPlayer {
    board: Board,
}

impl CliPlayer {
    pub(crate) fn new() -> Self {
        Self { board: Board::default() }
    }
}

impl Player for CliPlayer {
    fn name(&self) -> String {
        "human".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.board = Board::new_from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        m.apply(&mut self.board);
    }

    fn generate_move(&mut self) -> crate::Move {
        let mut moves = [None; 200];
        let n = Rules::generate_moves(&self.board, &mut moves);
        if moves[0] == Some(crate::Move::Pass) {
            return crate::Move::Pass;
        }
        loop {
            let line = read_line("move or place: ");
            if line.starts_with("move") {
                if let Some(m) = input_movement(&self.board, &moves[..n]) {
                    break m;
                }
            } else if line.starts_with("place") {
                if let Some(m) = input_placement(&self.board, &moves[..n]) {
                    break m;
                }
            }
        }
    }
}

pub fn terminal_game_interface() {
    let mut board = Board::default();
    let mut history = Vec::<crate::Move>::new();
    let mut strategy = IterativeSearch::<crate::BasicEvaluator>::new(
        IterativeOptions::new().with_table_byte_size(8_000_000).with_null_window_search(true),
    );
    loop {
        if let Some(winner) = Rules::get_winner(&board) {
            if winner == minimax::Winner::Draw {
                println!("{}Game over. Draw.", board);
            } else {
                println!("{}Game over.", board);
            }
            break;
        }
        // Precompute possible moves.
        let mut moves = [None; 200];
        let n = Rules::generate_moves(&mut board, &mut moves);
        if moves[0] == Some(crate::Move::Pass) {
            // Auto-pass if there are no valid moves.
            crate::Move::Pass.apply(&mut board);
            continue;
        }

        print!("{}{:?} to move", board, board.to_move());
        let line = read_line(": ");

        if line.starts_with("ai") {
            let mut depth = None;
            for arg in line.split(' ').skip(1) {
                if let Ok(num) = arg.parse::<usize>() {
                    depth = Some(num);
                }
            }
            if let Some(d) = depth {
                strategy.set_max_depth(d);
            } else {
                strategy.set_timeout(Duration::from_secs(5));
            }
            if let Some(m) = strategy.choose_move(&mut board) {
                history.push(m);
                m.apply(&mut board);
            }
            println!("{}", strategy.stats());
        } else if line.starts_with("move") {
            if let Some(m) = input_movement(&board, &moves[..n]) {
                history.push(m);
                m.apply(&mut board);
            }
        } else if line.starts_with("place") {
            if let Some(m) = input_placement(&board, &moves[..n]) {
                history.push(m);
                m.apply(&mut board);
            }
        } else if line.starts_with("undo") {
            if let Some(m) = history.pop() {
                m.undo(&mut board);
            }
        } else if line.starts_with("q") || line.starts_with("exit") {
            break;
        } else {
            println!("commands: ai, move, place, undo, quit");
        }
    }
}
