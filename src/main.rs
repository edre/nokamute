use hive;
use minimax::{Game, Move, Negamax, Options, Strategy};
use std::io::{self, BufRead, Write};

fn read_line(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    io::stdin().lock().lines().next().unwrap().unwrap()
}

fn input_id(board: &hive::Board, prompt: &str, options: &[hive::Id]) -> Option<hive::Id> {
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

fn input_bug(options: &[hive::Bug]) -> Option<hive::Bug> {
    for &bug in &[
        hive::Bug::Queen,
        hive::Bug::Grasshopper,
        hive::Bug::Spider,
        hive::Bug::Ant,
        hive::Bug::Beetle,
    ] {
        if options.contains(&bug) {
            print!(
                "{}:{}, ",
                bug.codepoint(),
                match bug {
                    hive::Bug::Queen => "queen",
                    hive::Bug::Grasshopper => "grasshopper",
                    hive::Bug::Spider => "spider",
                    hive::Bug::Ant => "ant",
                    hive::Bug::Beetle => "beetle",
                }
            );
        }
    }
    println!("");

    let line = read_line("Which bug? ");
    Some(match line.chars().next().unwrap_or('?') {
        'q' => hive::Bug::Queen,
        'g' => hive::Bug::Grasshopper,
        'h' => hive::Bug::Grasshopper,
        's' => hive::Bug::Spider,
        'a' => hive::Bug::Ant,
        'b' => hive::Bug::Beetle,
        _ => {
            println!("Unrecognized bug.");
            return None;
        }
    })
}

fn input_movement(board: &hive::Board, moves: &[Option<hive::Move>]) -> Option<hive::Move> {
    let mut starts =
        moves
            .iter()
            .filter_map(|m| {
                if let Some(hive::Move::Movement(start, _)) = m {
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
            if let Some(hive::Move::Movement(start2, end)) = m {
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

    Some(hive::Move::Movement(start, end))
}

fn input_placement(board: &hive::Board, moves: &[Option<hive::Move>]) -> Option<hive::Move> {
    let mut places = moves
        .iter()
        .filter_map(|m| if let Some(hive::Move::Place(place, _)) = m { Some(*place) } else { None })
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
        .filter_map(|m| if let Some(hive::Move::Place(_, bug)) = m { Some(*bug) } else { None })
        .collect::<Vec<_>>();
    let bug = input_bug(&bugs)?;

    Some(hive::Move::Place(place, bug))
}

fn main() {
    let mut board = hive::Board::default();
    let mut history = Vec::<hive::Move>::new();
    loop {
        if let Some(winner) = hive::Game::get_winner(&board) {
            if winner == minimax::Winner::Draw {
                println!("{}Game over. Draw.", board);
            } else {
                println!("{}Game over.", board);
            }
            break;
        }
        // Precompute possible moves.
        let mut moves = [None; 200];
        let n = hive::Game::generate_moves(&mut board, &mut moves);
        if moves[0] == Some(hive::Move::Pass) {
            // Auto-pass if there are no valid moves.
            hive::Move::Pass.apply(&mut board);
            continue;
        }

        print!("{}{:?} to move", board, board.to_move());
        let line = read_line(": ");

        if line.starts_with("ai") {
            let mut depth = 4;
            for arg in line.split(' ').skip(1) {
                if let Ok(num) = arg.parse::<usize>() {
                    depth = num;
                }
            }
            let mut strategy = Negamax::<hive::BasicEvaluator>::new(Options { max_depth: depth });
            if let Some(m) = strategy.choose_move(&mut board) {
                history.push(m);
                m.apply(&mut board);
            }
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
