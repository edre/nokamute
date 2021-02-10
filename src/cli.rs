use crate::board;
use crate::board::{Board, Bug, Id};
use crate::strategies::*;
use minimax::{Game, Move, Strategy};
use std::io::{self, BufRead, Write};
use std::time::Duration;

fn read_line(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    io::stdin().lock().lines().next().unwrap().unwrap()
}

fn input_id(board: &Board, prompt: &str, options: &[Id]) -> Option<Id> {
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
    for &bug in &[
        Bug::Queen,
        Bug::Grasshopper,
        Bug::Spider,
        Bug::Ant,
        Bug::Beetle,
        Bug::Mosquito,
        Bug::Ladybug,
        Bug::Pillbug,
    ] {
        if options.contains(&bug) {
            print!(
                "{}:{}, ",
                bug.codepoint(),
                match bug {
                    Bug::Queen => "queen",
                    Bug::Grasshopper => "grasshopper",
                    Bug::Spider => "spider",
                    Bug::Ant => "ant",
                    Bug::Beetle => "beetle",
                    Bug::Mosquito => "mosquito",
                    Bug::Ladybug => "ladybug",
                    Bug::Pillbug => "pillbug",
                }
            );
        }
    }
    println!("");

    let line = read_line("Which bug? ");
    Some(match line.chars().next().unwrap_or('?') {
        'q' => Bug::Queen,
        'g' => Bug::Grasshopper,
        'h' => Bug::Grasshopper,
        's' => Bug::Spider,
        'a' => Bug::Ant,
        'b' => Bug::Beetle,
        'm' => Bug::Mosquito,
        'l' => Bug::Ladybug,
        'p' => Bug::Pillbug,
        _ => {
            println!("Unrecognized bug.");
            return None;
        }
    })
}

fn input_movement(board: &Board, moves: &[Option<board::Move>]) -> Option<board::Move> {
    let mut starts =
        moves
            .iter()
            .filter_map(|m| {
                if let Some(board::Move::Movement(start, _)) = m {
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
    let start = input_id(board, "Move which bug? ", &starts)?;

    let mut ends = moves
        .iter()
        .filter_map(|m| {
            if let Some(board::Move::Movement(start2, end)) = m {
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
    let end = input_id(board, "Move to where? ", &ends)?;

    Some(board::Move::Movement(start, end))
}

fn input_placement(board: &Board, moves: &[Option<board::Move>]) -> Option<board::Move> {
    let mut places = moves
        .iter()
        .filter_map(
            |m| if let Some(board::Move::Place(place, _)) = m { Some(*place) } else { None },
        )
        .collect::<Vec<_>>();
    places.sort();
    places.dedup();
    if places.is_empty() {
        println!("No placements available.");
        return None;
    }
    let place = input_id(board, "Place new bug where? ", &places)?;

    let bugs = moves
        .iter()
        .filter_map(|m| if let Some(board::Move::Place(_, bug)) = m { Some(*bug) } else { None })
        .collect::<Vec<_>>();
    let bug = input_bug(&bugs)?;

    Some(board::Move::Place(place, bug))
}

pub fn terminal_game_interface() {
    let mut board = Board::default();
    let mut history = Vec::<board::Move>::new();
    let mut strategy = IterativeSearch::<board::BasicEvaluator>::new(
        IterativeOptions::default().with_table_size(200_000),
    );
    loop {
        if let Some(winner) = board::Game::get_winner(&board) {
            if winner == minimax::Winner::Draw {
                println!("{}Game over. Draw.", board);
            } else {
                println!("{}Game over.", board);
            }
            break;
        }
        // Precompute possible moves.
        let mut moves = [None; 200];
        let n = board::Game::generate_moves(&mut board, &mut moves);
        if moves[0] == Some(board::Move::Pass) {
            // Auto-pass if there are no valid moves.
            board::Move::Pass.apply(&mut board);
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
                strategy.max_depth = d;
                strategy.max_time = Duration::new(30, 0);
            } else {
                strategy.max_depth = 50;
                strategy.max_time = Duration::new(5, 0);
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
