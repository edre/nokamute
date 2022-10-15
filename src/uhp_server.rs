extern crate minimax;

use crate::notation::{Result, UhpError};
use crate::{Board, Player, PlayerConfig, Rules};

use std::io::Write;
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
use std::io::{stdin, stdout};
use std::time::Duration;

pub struct UhpServer<W: Write> {
    board: Option<Board>,
    config: PlayerConfig,
    engine: Option<Box<dyn Player>>,
    output: W,
}

impl<W: Write> UhpServer<W> {
    pub fn new(config: PlayerConfig, output: W) -> Self {
        UhpServer { board: None, config, engine: None, output }
    }

    pub fn swap_output(&mut self, mut output: W) -> W {
        std::mem::swap(&mut output, &mut self.output);
        output
    }

    fn info(&mut self) -> Result<()> {
        // Version string
        writeln!(self.output, "id {} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))?;
        // Capabilities
        writeln!(self.output, "Mosquito;Ladybug;Pillbug")?;
        Ok(())
    }

    fn new_game(&mut self, args: &str) -> Result<()> {
        let args = if args.is_empty() { "Base" } else { args };
        self.board = Some(Board::from_game_string(args)?);
        let mut engine = self.config.new_player();
        engine.new_game(args);
        self.engine = Some(engine);
        writeln!(self.output, "{}", self.board.as_mut().unwrap().game_string())?;
        Ok(())
    }

    fn valid_moves(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "{}",
            self.board.as_ref().ok_or(UhpError::GameNotStarted)?.valid_moves()
        )?;
        Ok(())
    }

    fn play(&mut self, args: &str) -> Result<()> {
        let board = self.board.as_mut().ok_or(UhpError::GameNotStarted)?;
        let m = board.from_move_string(args)?;
        board.apply_untrusted(m)?;
        self.engine.as_mut().unwrap().play_move(m);
        writeln!(self.output, "{}", board.game_string())?;
        Ok(())
    }

    fn best_move(&mut self, args: &str) -> Result<()> {
        let board = self.board.as_ref().ok_or(UhpError::GameNotStarted)?;
        if let Some(arg) = args.strip_prefix("depth ") {
            let depth =
                arg.parse::<u8>().map_err(|_| UhpError::UnrecognizedCommand(args.to_string()))?;
            self.engine.as_mut().unwrap().set_max_depth(depth);
        } else if let Some(arg) = args.strip_prefix("time ") {
            let dur =
                parse_hhmmss(arg).ok_or_else(|| UhpError::UnrecognizedCommand(args.to_string()))?;
            self.engine.as_mut().unwrap().set_timeout(dur);
        } else {
            return Err(UhpError::UnrecognizedCommand(args.to_string()));
        }
        let m = self.engine.as_mut().unwrap().generate_move();
        writeln!(self.output, "{}", board.to_move_string(m))?;
        Ok(())
    }

    fn undo(&mut self, args: &str) -> Result<()> {
        let board = self.board.as_mut().ok_or(UhpError::GameNotStarted)?;
        let num_undo = if args.is_empty() {
            1
        } else {
            args.parse::<usize>().map_err(|_| UhpError::UnrecognizedCommand(args.to_string()))?
        };
        if num_undo > board.move_history.len() {
            return Err(UhpError::TooManyUndos);
        }
        for _ in 0..num_undo {
            self.engine.as_mut().unwrap().undo_move(board.last_move().unwrap());
            board.undo_count(1)?;
        }
        writeln!(self.output, "{}", board.game_string())?;
        Ok(())
    }

    fn options(&mut self, _args: &str) -> Result<()> {
        // unimplemented, but the idea is to mutate PlayerConfig
        Ok(())
    }

    // Bonus undocumented command.
    fn perft(&mut self, args: &str) -> Result<()> {
        let depth = args.parse::<u8>().unwrap_or(20);
        let mut b = self.board.as_ref().ok_or(UhpError::GameNotStarted)?.clone();
        minimax::perft::<Rules>(&mut b, depth, false);
        Ok(())
    }

    pub fn command(&mut self, line: &str) -> bool {
        let line = line.trim();
        let space = line.find(' ');
        let command = if let Some(i) = space { &line[..i] } else { line };
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
            "exit" => return true,
            _ => Err(UhpError::UnrecognizedCommand(command.to_string())),
        };
        if let Err(err) = result {
            if let UhpError::InvalidMove(invalid) = err {
                writeln!(self.output, "invalidmove {}", invalid).unwrap();
            } else {
                writeln!(self.output, "err {:?}", err).unwrap();
            }
        }
        false
    }
}

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub fn uhp_serve(config: PlayerConfig) {
    let mut server = UhpServer::new(config, stdout());
    server.info().unwrap();
    println!("ok");
    loop {
        let mut line = String::new();
        match stdin().read_line(&mut line) {
            Ok(size) => {
                if size == 0 {
                    return;
                }
            }
            Err(err) => {
                eprintln!("{}", err);
                return;
            }
        };
        if server.command(&line) {
            return;
        }
        println!("ok");
    }
}

fn parse_hhmmss(time: &str) -> Option<Duration> {
    let mut toks = time.split(':');
    let hours = toks.next().unwrap_or("").parse::<u64>().ok()?;
    let minutes = toks.next().unwrap_or("").parse::<u64>().ok()?;
    let seconds = toks.next().unwrap_or("").parse::<u64>().ok()?;
    Some(Duration::from_secs(hours * 3600 + minutes * 60 + seconds))
}
