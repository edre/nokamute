extern crate minimax;

use crate::uhp_util::{Result, UhpBoard, UhpError};
use crate::{BasicEvaluator, Rules};

use minimax::Strategy;
use std::io::{stderr, stdin, Write};
use std::time::Duration;

pub struct UhpServer {
    board: Option<UhpBoard>,
    options: minimax::IterativeOptions,
    engine: Option<minimax::IterativeSearch<BasicEvaluator>>,
}

impl UhpServer {
    pub fn new() -> Self {
        let server =
            UhpServer { board: None, options: minimax::IterativeOptions::new(), engine: None };
        server.info().unwrap();
        println!("ok");
        server
    }

    fn info(&self) -> Result<()> {
        // Version string
        println!("id {} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
        // Capabilities
        println!("Mosquito;Ladybug;Pillbug");
        Ok(())
    }

    fn new_game(&mut self, args: &str) -> Result<()> {
        let args = if args.is_empty() { "Base" } else { args };
        self.board = Some(UhpBoard::from_game_string(args)?);
        self.engine = Some(minimax::IterativeSearch::new(BasicEvaluator::default(), self.options));
        println!("{}", self.board.as_mut().unwrap().game_string());
        Ok(())
    }

    fn valid_moves(&self) -> Result<()> {
        println!("{}", self.board.as_ref().ok_or(UhpError::GameNotStarted)?.valid_moves());
        Ok(())
    }

    fn play(&mut self, args: &str) -> Result<()> {
        let board = self.board.as_mut().ok_or(UhpError::GameNotStarted)?;
        let m = board.from_move_string(args)?;
        board.apply_untrusted(m)?;
        println!("{}", board.game_string());
        Ok(())
    }

    fn best_move(&mut self, args: &str) -> Result<()> {
        let strategy = self.engine.as_mut().ok_or(UhpError::GameNotStarted)?;
        if let Some(arg) = args.strip_prefix("depth ") {
            let depth = arg
                .parse::<usize>()
                .map_err(|_| UhpError::UnrecognizedCommand(args.to_string()))?;
            strategy.set_max_depth(depth);
        } else if let Some(arg) = args.strip_prefix("time ") {
            let dur =
                parse_hhmmss(arg).ok_or_else(|| UhpError::UnrecognizedCommand(args.to_string()))?;
            strategy.set_timeout(dur);
        } else {
            return Err(UhpError::UnrecognizedCommand(args.to_string()));
        }
        let board = self.board.as_ref().unwrap();
        let m = strategy.choose_move(board.inner()).unwrap();
        println!("{}", board.to_move_string(m));
        stderr().write_all((strategy.stats() + "\n").as_bytes())?;
        Ok(())
    }

    fn undo(&mut self, args: &str) -> Result<()> {
        let board = self.board.as_mut().ok_or(UhpError::GameNotStarted)?;
        let num_undo = if args.is_empty() {
            1
        } else {
            args.parse::<usize>().map_err(|_| UhpError::UnrecognizedCommand(args.to_string()))?
        };
        for _ in 0..num_undo {
            board.undo()?;
        }
        println!("{}", board.game_string());
        Ok(())
    }

    fn options(&mut self, _args: &str) -> Result<()> {
        // unimplemented
        Ok(())
    }

    // Bonus undocumented command.
    fn perft(&mut self, args: &str) -> Result<()> {
        let depth = args.parse::<usize>().unwrap_or(20);
        let mut b = self.board.as_ref().ok_or(UhpError::GameNotStarted)?.inner().clone();
        minimax::perft::<Rules>(&mut b, depth);
        Ok(())
    }

    pub fn serve(&mut self) {
        loop {
            let mut line = String::new();
            stdin().read_line(&mut line).unwrap();
            let line = line.trim();
            let space = line.find(' ');
            let command = if let Some(i) = space { &line[..i] } else { &line };
            let args = if let Some(i) = space { &line[i + 1..] } else { "" };
            let result = match command {
                "info" => self.info(),
                "newgame" => self.new_game(args),
                "validmoves" => self.valid_moves(),
                "play" => self.play(args),
                "pass" => self.play("pass"),
                "bestmove" => self.best_move(args),
                "undo" => self.undo(args),
                "options" => self.options(args),
                "perft" => self.perft(args),
                "exit" => return,
                _ => Err(UhpError::UnrecognizedCommand(command.to_string())),
            };
            if let Err(err) = result {
                if let UhpError::InvalidMove(invalid) = err {
                    println!("invalidmove {}", invalid);
                } else {
                    println!("err {:?}", err);
                }
            }
            println!("ok");
        }
    }
}

fn parse_hhmmss(time: &str) -> Option<Duration> {
    let mut toks = time.split(':');
    let hours = toks.next().unwrap_or("").parse::<u64>().ok()?;
    let minutes = toks.next().unwrap_or("").parse::<u64>().ok()?;
    let seconds = toks.next().unwrap_or("").parse::<u64>().ok()?;
    Some(Duration::from_secs(hours * 3600 + minutes * 60 + seconds))
}
