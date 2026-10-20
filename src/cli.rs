use crate::Rules;
use crate::player::{Player, PlayerConfig};
use hexbugs::{Bug, GameState, Hex, Turn, Winner};
use minimax::{Game, Strategy};
use std::io::{self, BufRead, Write};
use std::time::Duration;

fn read_line(prompt: &str) -> String {
    print!("{prompt}");
    io::stdout().flush().unwrap();
    if let Some(input) = io::stdin().lock().lines().next() {
        input.unwrap()
    } else {
        println!();
        std::process::exit(0);
    }
}

fn input_hex(game: &GameState, prompt: &str, options: &[Hex]) -> Option<Hex> {
    game.println_highlights(options);
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
    println!();

    let line = read_line("Which bug? ");
    let bug = Bug::from_char(line.chars().next().unwrap_or('?'));
    if let Some(bug) = bug {
        if !options.contains(&bug) {
            println!("Bug not available.");
            None
        } else {
            Some(bug)
        }
    } else {
        println!("Unrecognized bug.");
        None
    }
}

fn input_movement(game: &GameState, moves: &[Turn]) -> Option<Turn> {
    let mut starts = moves
        .iter()
        .filter_map(|m| if let Turn::Move(start, _) = m { Some(*start) } else { None })
        .collect::<Vec<_>>();
    starts.sort_unstable();
    starts.dedup();
    if starts.is_empty() {
        println!("No movements available.");
        return None;
    }
    let start = input_hex(game, "Move which bug? ", &starts)?;

    let mut ends = moves
        .iter()
        .filter_map(|m| {
            if let Turn::Move(start2, end) = m {
                if start == *start2 { Some(*end) } else { None }
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    ends.sort_unstable();
    ends.dedup();
    let end = input_hex(game, "Move to where? ", &ends)?;

    Some(Turn::Move(start, end))
}

fn input_placement(game: &GameState, moves: &[Turn]) -> Option<Turn> {
    let mut places = moves
        .iter()
        .filter_map(|m| if let Turn::Place(place, _) = m { Some(*place) } else { None })
        .collect::<Vec<_>>();
    places.sort_unstable();
    places.dedup();
    if places.is_empty() {
        println!("No placements available.");
        return None;
    }
    let place = input_hex(game, "Place new bug where? ", &places)?;

    let bugs = moves
        .iter()
        .filter_map(|m| if let Turn::Place(_, bug) = m { Some(*bug) } else { None })
        .collect::<Vec<_>>();
    let bug = input_bug(&bugs)?;

    Some(Turn::Place(place, bug))
}

pub(crate) struct CliPlayer {
    game: GameState,
}

impl CliPlayer {
    pub(crate) fn new() -> Self {
        Self { game: GameState::default() }
    }
}

impl Player for CliPlayer {
    fn name(&self) -> String {
        "human".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.game = GameState::from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, turn: Turn) {
        self.game.apply(turn)
    }

    fn undo_move(&mut self, turn: Turn) {
        self.game.undo(turn);
    }

    fn generate_move(&mut self) -> Turn {
        let mut moves = Vec::new();
        Rules::generate_moves(&self.game, &mut moves);
        if moves[0] == Turn::Pass {
            return Turn::Pass;
        }
        loop {
            let line = read_line("move or place: ");
            if line.starts_with("move") {
                if let Some(m) = input_movement(&self.game, &moves) {
                    break m;
                }
            } else if line.starts_with("place")
                && let Some(m) = input_placement(&self.game, &moves)
            {
                break m;
            }
        }
    }
}

pub fn terminal_game_interface(config: PlayerConfig) {
    let mut player = config.new_player();
    let mut game = GameState::default();
    let mut history = Vec::<Turn>::new();
    let mut prev_pv = Vec::new();
    let mut prev_pv_game = GameState::default();
    loop {
        if let Some(winner) = game.get_winner() {
            game.println();
            if winner == Winner::Draw {
                println!("Game over. Draw.");
            } else {
                println!("Game over.");
            }
            break;
        }
        // Precompute possible moves.
        let mut moves = Vec::new();
        game.generate_moves(&mut moves);
        if moves[0] == Turn::Pass {
            // Auto-pass if there are no valid moves.
            game.apply(Turn::Pass);
            continue;
        }

        game.println();
        print!("{:?} to move", game.to_move());
        let line = read_line(": ");

        if line.starts_with("ai") {
            let mut depth = None;
            for arg in line.split(' ').skip(1) {
                if let Ok(num) = arg.parse::<u8>() {
                    depth = Some(num);
                }
            }
            if let Some(d) = depth {
                player.set_max_depth(d);
            } else {
                player.set_timeout(Duration::from_secs(5));
            }
            let m = player.generate_move();
            player.play_move(m);
            prev_pv_game = game.clone();
            prev_pv = player.principal_variation();
            history.push(m);
            game.apply(m);
        } else if line.starts_with("mcts") {
            let opts = minimax::MCTSOptions::default().with_max_rollout_depth(200);
            let mut mcts = minimax::MonteCarloTreeSearch::<Rules>::new(opts);
            for arg in line.split(' ').skip(1) {
                if let Ok(num) = arg.parse::<u32>() {
                    mcts.set_max_rollouts(num);
                }
            }
            if let Some(m) = mcts.choose_move(&game) {
                history.push(m);
                game.apply(m);
                player.play_move(m);
            }
        } else if line.starts_with("move") {
            if let Some(m) = input_movement(&game, &moves) {
                history.push(m);
                game.apply(m);
                player.play_move(m);
            }
        } else if line.starts_with("place") {
            if let Some(m) = input_placement(&game, &moves) {
                history.push(m);
                game.apply(m);
                player.play_move(m);
            }
        } else if line.starts_with("pass") {
            history.push(Turn::Pass);
            game.apply(Turn::Pass);
            player.play_move(Turn::Pass);
        } else if line.starts_with("undo") {
            if let Some(m) = history.pop() {
                game.undo(m);
                player.undo_move(m);
            }
        } else if line.starts_with("pv") {
            for (i, &m) in prev_pv.iter().enumerate() {
                prev_pv_game.apply(m);
                if i > 0 {
                    println!("Principal variation depth {i}");
                    prev_pv_game.println();
                }
            }
            for &m in prev_pv.iter().rev() {
                prev_pv_game.undo(m);
            }
            println!("Current game:");
        } else if line.starts_with("newgame") {
            let game_string = read_line("Game type? ");
            game = if let Ok(b) = GameState::from_game_string(&game_string) {
                b
            } else {
                println!("Invalid game string");
                continue;
            };
            player.new_game(&game_string);
        } else if line.starts_with('q') || line.starts_with("exit") {
            break;
        } else {
            println!("commands: ai, pv, move, place, pass, undo, quit");
        }
    }
}
