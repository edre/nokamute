extern crate termcolor;

use crate::player::{Player, PlayerConfig};
use crate::{Board, Bug, Color, Hex, Rules, Turn, ROW_SIZE, START_HEX};
use minimax::{Game, Move, Strategy};
use std::io::{self, BufRead, Write};
use std::time::Duration;
use termcolor::WriteColor;

impl Board {
    // return the Hex to the upper left and lower right of all occupied nodes.
    // Given wrapping, the second may be less than the first.
    fn bounding_box(&self) -> (Hex, Hex, Hex, Hex) {
        let empty_rows = (0..ROW_SIZE)
            .map(|r| ((0..ROW_SIZE).all(|c| !self.occupied(r * ROW_SIZE + c))))
            .collect::<Vec<bool>>();
        let empty_cols = (0..ROW_SIZE)
            .map(|c| ((0..ROW_SIZE).all(|r| !self.occupied(r * ROW_SIZE + c))))
            .collect::<Vec<bool>>();
        if empty_rows.iter().all(|&r| r) {
            // Center around start hex
            return (
                START_HEX / ROW_SIZE - 1,
                START_HEX / ROW_SIZE + 1,
                START_HEX % ROW_SIZE - 1,
                START_HEX % ROW_SIZE + 1,
            );
        }
        let mut minr = 0;
        let mut maxr = 0;
        let mut minc = 0;
        let mut maxc = 0;
        for i in 0..ROW_SIZE as usize {
            let j = (i + 1) % ROW_SIZE as usize;
            if empty_rows[i] && !empty_rows[j] {
                minr = i as Hex;
            }
            if !empty_rows[i] && empty_rows[j] {
                maxr = j as Hex;
            }
            if empty_cols[i] && !empty_cols[j] {
                minc = i as Hex;
            }
            if !empty_cols[i] && empty_cols[j] {
                maxc = j as Hex;
            }
        }
        (minr, maxr, minc, maxc)
    }

    pub fn fancy_fmt(
        &self, buf: &mut termcolor::Buffer, highlights: &[Hex],
    ) -> std::io::Result<()> {
        let (startr, endr, startc, endc) = self.bounding_box();
        let free_space = "\u{ff0e}".as_bytes();

        let mut r = startr;
        while r != (endr + 1) % ROW_SIZE {
            // Print prefix to get staggered hex rows
            let buflen = endr.wrapping_sub(r) % ROW_SIZE;
            if buflen % 2 == 1 {
                buf.write_all(b" ")?;
            }
            for _ in 0..buflen / 2 {
                buf.write_all(free_space)?;
            }

            let mut c = startc;
            while c != (endc + 1) % ROW_SIZE {
                let hex = c + r * ROW_SIZE;
                if let Some(index) = highlights.iter().position(|&x| x == hex) {
                    write!(buf, "{: >2}", index)?;
                    c = (c + 1) % ROW_SIZE;
                    continue;
                }
                let node = self.node(hex);
                if node.occupied() {
                    if node.color() == Color::White {
                        // Invert terminal background color for white pieces.
                        buf.set_color(
                            termcolor::ColorSpec::new().set_bg(Some(termcolor::Color::White)),
                        )?;
                    }
                    write!(buf, "{}", node.bug().codepoint())?;
                    if node.color() == Color::White {
                        // Reset coloring.
                        buf.reset()?;
                    }
                } else {
                    // Empty cell. Full width period.
                    buf.write_all(free_space)?;
                }
                c = (c + 1) % ROW_SIZE;
            }

            // Stagger rows the other way to make the space look rectangular.
            for _ in 0..r.wrapping_sub(startr) % ROW_SIZE / 2 {
                buf.write_all(free_space)?;
            }

            // On 2nd and 3rd rows, print remaining bugs from each side
            if r == (startr + 1) % ROW_SIZE {
                buf.write_all(b" ")?;
                self.write_remaining(Color::White, buf)?;
            } else if r == (startr + 2) % ROW_SIZE {
                self.write_remaining(Color::Black, buf)?;
            }

            buf.write_all(b"\n")?;
            r = (r + 1) % ROW_SIZE;
        }
        Ok(())
    }

    fn write_remaining(&self, color: Color, buf: &mut termcolor::Buffer) -> std::io::Result<()> {
        for bug in Bug::iter_all() {
            let count = self.remaining[color as usize][bug as usize];
            let prefix = if count == 2 {
                b"2"
            } else if count == 3 {
                b"3"
            } else {
                b" "
            };
            if count > 0 {
                buf.write_all(prefix)?;
                if color == Color::White {
                    // Invert terminal background color for white pieces.
                    buf.set_color(
                        termcolor::ColorSpec::new().set_bg(Some(termcolor::Color::White)),
                    )?;
                }
                write!(buf, "{}", bug.codepoint())?;
                if color == Color::White {
                    // Reset coloring.
                    buf.reset()?;
                }
                buf.write_all(b" ")?;
            }
        }
        Ok(())
    }

    pub(crate) fn println(&self) {
        self.println_highlights(&[]);
    }

    pub(crate) fn println_highlights(&self, highlights: &[Hex]) {
        let writer = termcolor::BufferWriter::stdout(termcolor::ColorChoice::Auto);
        let mut buffer = writer.buffer();
        self.fancy_fmt(&mut buffer, highlights).unwrap();
        writer.print(&buffer).unwrap();
    }
}

fn read_line(prompt: &str) -> String {
    print!("{}", prompt);
    io::stdout().flush().unwrap();
    if let Some(input) = io::stdin().lock().lines().next() {
        input.unwrap()
    } else {
        println!();
        std::process::exit(0);
    }
}

fn input_hex(board: &Board, prompt: &str, options: &[Hex]) -> Option<Hex> {
    board.println_highlights(options);
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

fn input_movement(board: &Board, moves: &[Turn]) -> Option<Turn> {
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
    let start = input_hex(board, "Move which bug? ", &starts)?;

    let mut ends = moves
        .iter()
        .filter_map(|m| {
            if let Turn::Move(start2, end) = m {
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
    ends.sort_unstable();
    ends.dedup();
    let end = input_hex(board, "Move to where? ", &ends)?;

    Some(Turn::Move(start, end))
}

fn input_placement(board: &Board, moves: &[Turn]) -> Option<Turn> {
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
    let place = input_hex(board, "Place new bug where? ", &places)?;

    let bugs = moves
        .iter()
        .filter_map(|m| if let Turn::Place(_, bug) = m { Some(*bug) } else { None })
        .collect::<Vec<_>>();
    let bug = input_bug(&bugs)?;

    Some(Turn::Place(place, bug))
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
        self.board = Board::from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: Turn) {
        m.apply(&mut self.board);
    }

    fn undo_move(&mut self, m: Turn) {
        m.undo(&mut self.board);
    }

    fn generate_move(&mut self) -> Turn {
        let mut moves = Vec::new();
        Rules::generate_moves(&self.board, &mut moves);
        if moves[0] == Turn::Pass {
            return Turn::Pass;
        }
        loop {
            let line = read_line("move or place: ");
            if line.starts_with("move") {
                if let Some(m) = input_movement(&self.board, &moves) {
                    break m;
                }
            } else if line.starts_with("place") {
                if let Some(m) = input_placement(&self.board, &moves) {
                    break m;
                }
            }
        }
    }
}

pub fn terminal_game_interface(config: PlayerConfig) {
    let mut player = config.new_player();
    let mut board = Board::default();
    let mut history = Vec::<Turn>::new();
    let mut prev_pv = Vec::new();
    let mut prev_pv_board = Board::default();
    loop {
        if let Some(winner) = Rules::get_winner(&board) {
            board.println();
            if winner == minimax::Winner::Draw {
                println!("Game over. Draw.");
            } else {
                println!("Game over.");
            }
            break;
        }
        // Precompute possible moves.
        let mut moves = Vec::new();
        Rules::generate_moves(&board, &mut moves);
        if moves[0] == Turn::Pass {
            // Auto-pass if there are no valid moves.
            Turn::Pass.apply(&mut board);
            continue;
        }

        board.println();
        print!("{:?} to move", board.to_move());
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
            prev_pv_board = board.clone();
            prev_pv = player.principal_variation();
            history.push(m);
            m.apply(&mut board);
        } else if line.starts_with("mcts") {
            let opts = minimax::MCTSOptions::default().with_max_rollout_depth(200);
            let mut mcts = minimax::MonteCarloTreeSearch::<Rules>::new(opts);
            for arg in line.split(' ').skip(1) {
                if let Ok(num) = arg.parse::<u32>() {
                    mcts.set_max_rollouts(num);
                }
            }
            if let Some(m) = mcts.choose_move(&board) {
                history.push(m);
                m.apply(&mut board);
                player.play_move(m);
            }
        } else if line.starts_with("move") {
            if let Some(m) = input_movement(&board, &moves) {
                history.push(m);
                m.apply(&mut board);
                player.play_move(m);
            }
        } else if line.starts_with("place") {
            if let Some(m) = input_placement(&board, &moves) {
                history.push(m);
                m.apply(&mut board);
                player.play_move(m);
            }
        } else if line.starts_with("pass") {
            history.push(Turn::Pass);
            Turn::Pass.apply(&mut board);
            player.play_move(Turn::Pass);
        } else if line.starts_with("undo") {
            if let Some(m) = history.pop() {
                m.undo(&mut board);
                player.undo_move(m);
            }
        } else if line.starts_with("pv") {
            for (i, m) in prev_pv.iter().enumerate() {
                m.apply(&mut prev_pv_board);
                if i > 0 {
                    println!("Principal variation depth {}", i);
                    prev_pv_board.println();
                }
            }
            for m in prev_pv.iter().rev() {
                m.undo(&mut prev_pv_board);
            }
            println!("Current board:");
        } else if line.starts_with("newgame") {
            let game_string = read_line("Game type? ");
            board = if let Ok(b) = Board::from_game_string(&game_string) {
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
