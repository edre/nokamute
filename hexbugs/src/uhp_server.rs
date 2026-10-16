use crate::{GameState, Result, Turn, UhpError};
use std::io::Write;
#[cfg(not(target_arch = "wasm32"))]
use std::io::{stdin, stdout};
use std::time::Duration;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BestMoveLimit {
    Depth(u8),
    Time(Duration),
    DepthOrTime(u8, Duration),
}

pub trait UhpEngine {
    fn name(&self) -> &str;
    fn version(&self) -> &str {
        ""
    }
    fn capabilities(&self) -> &[&str] {
        &["Mosquito", "Ladybug", "Pillbug"]
    }
    fn best_move(&mut self, game: &GameState, limit: BestMoveLimit) -> Turn;
    fn principal_variation(&self) -> Vec<Turn> {
        Vec::new()
    }

    fn get_options(&self) -> Vec<String> {
        Vec::new()
    }
    fn set_option(&mut self, name: &str, value: &str) -> Result<String> {
        let _ = value;
        Err(UhpError::InvalidOption(name.to_string()))
    }
}

pub struct UhpServer<E: UhpEngine, W: Write> {
    game: Option<GameState>,
    pv_dirty: bool,
    pub engine: E,
    output: W,
}

impl<E: UhpEngine, W: Write> UhpServer<E, W> {
    pub fn new(engine: E, output: W) -> Self {
        UhpServer {
            game: None,
            pv_dirty: true,
            engine,
            output,
        }
    }

    pub fn game(&self) -> Option<&GameState> {
        self.game.as_ref()
    }

    pub fn swap_output(&mut self, mut output: W) -> W {
        std::mem::swap(&mut output, &mut self.output);
        output
    }

    fn info(&mut self) -> Result<()> {
        let version = self.engine.version();
        if version.is_empty() {
            writeln!(self.output, "id {}", self.engine.name())?;
        } else {
            writeln!(self.output, "id {} {}", self.engine.name(), version)?;
        }
        writeln!(self.output, "{}", self.engine.capabilities().join(";"))?;
        Ok(())
    }

    fn new_game(&mut self, args: &str) -> Result<()> {
        self.pv_dirty = true;
        let args = if args.is_empty() { "Base" } else { args };
        self.game = Some(GameState::from_game_string(args)?);
        writeln!(self.output, "{}", self.game.as_mut().unwrap().game_string())?;
        Ok(())
    }

    fn valid_moves(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "{}",
            self.game.as_ref().ok_or(UhpError::GameNotStarted)?.valid_moves()
        )?;
        Ok(())
    }

    fn play(&mut self, args: &str) -> Result<()> {
        self.pv_dirty = true;
        let game = self.game.as_mut().ok_or(UhpError::GameNotStarted)?;
        let m = game.from_move_string(args)?;
        game.apply_untrusted(m)?;
        writeln!(self.output, "{}", game.game_string())?;
        Ok(())
    }

    fn best_move(&mut self, args: &str) -> Result<()> {
        self.pv_dirty = false;
        let game = self.game.as_ref().ok_or(UhpError::GameNotStarted)?;
        if game.get_winner().is_some() {
            return Err(UhpError::EngineError("the game is already over".to_string()));
        }
        let arg_error = || UhpError::UnrecognizedCommand(args.to_string());
        let limit = if let Some(arg) = args.strip_prefix("depth ") {
            let depth = arg.parse::<u8>().map_err(|_| arg_error())?;
            if depth == 0 {
                return Err(UhpError::EngineError("depth must be positive".to_string()));
            }
            BestMoveLimit::Depth(depth)
        } else if let Some(arg) = args.strip_prefix("time ") {
            let dur = parse_hhmmss(arg).ok_or_else(arg_error)?;
            BestMoveLimit::Time(dur)
        } else if let Some(arg) = args.strip_prefix("seconds ") {
            let dur = parse_seconds(arg).ok_or_else(arg_error)?;
            BestMoveLimit::Time(dur)
        } else if let Some(arg) = args.strip_prefix("depthorseconds ") {
            let mut toks = arg.split(' ');
            let depth =
                toks.next().ok_or_else(arg_error)?.parse::<u8>().map_err(|_| arg_error())?;
            if depth == 0 {
                return Err(UhpError::EngineError("depth must be positive".to_string()));
            }
            let dur = parse_seconds(toks.next().ok_or_else(arg_error)?).ok_or_else(arg_error)?;
            if toks.next().is_some() {
                return Err(arg_error());
            }
            BestMoveLimit::DepthOrTime(depth, dur)
        } else {
            return Err(arg_error());
        };
        let m = self.engine.best_move(game, limit);
        writeln!(self.output, "{}", game.to_move_string(m))?;
        Ok(())
    }

    fn pv(&mut self) -> Result<()> {
        let game = self.game.as_mut().ok_or(UhpError::GameNotStarted)?;
        if self.pv_dirty {
            return Err(UhpError::EngineError("game changed since last engine move".into()));
        }
        let pv = self.engine.principal_variation();
        for &m in &pv {
            writeln!(self.output, "{}", game.to_move_string(m))?;
            game.apply(m);
        }
        for &m in pv.iter().rev() {
            game.undo(m);
        }
        Ok(())
    }

    fn undo(&mut self, args: &str) -> Result<()> {
        self.pv_dirty = true;
        let game = self.game.as_mut().ok_or(UhpError::GameNotStarted)?;
        let num_undo = if args.is_empty() {
            1
        } else {
            args.parse::<usize>().map_err(|_| UhpError::UnrecognizedCommand(args.to_string()))?
        };
        if num_undo > game.turn_history.len() {
            return Err(UhpError::TooManyUndos);
        }
        game.undo_count(num_undo)?;
        writeln!(self.output, "{}", game.game_string())?;
        Ok(())
    }

    fn options(&mut self, args: &str) -> Result<()> {
        let args = args.trim();
        if args.is_empty() {
            for line in self.engine.get_options() {
                writeln!(self.output, "{line}")?;
            }
        } else {
            let tokens = args.split(' ').collect::<Vec<_>>();
            if tokens.len() == 2 && tokens[0] == "get" {
                let name = tokens[1];
                let prefix = format!("{name};");
                let opt = self.engine.get_options().into_iter().find(|o| o.starts_with(&prefix));
                if let Some(opt_line) = opt {
                    writeln!(self.output, "{opt_line}")?;
                } else {
                    return Err(UhpError::InvalidOption(name.into()));
                }
            } else if tokens.len() == 3 && tokens[0] == "set" {
                let name = tokens[1];
                let val = tokens[2];
                let updated = self.engine.set_option(name, val)?;
                writeln!(self.output, "{updated}")?;
            } else {
                return Err(UhpError::UnrecognizedCommand(args.into()));
            }
        }
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
            "pv" => self.pv(),
            "undo" => self.undo(args),
            "options" => self.options(args),
            "exit" => return true,
            _ => Err(UhpError::UnrecognizedCommand(command.to_string())),
        };
        if let Err(err) = result {
            if let UhpError::InvalidMove(invalid) = err {
                writeln!(self.output, "invalidmove {invalid}").unwrap();
            } else {
                writeln!(self.output, "err {err:?}").unwrap();
            }
        }
        false
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn uhp_serve<E: UhpEngine>(engine: E) {
    let mut server = UhpServer::new(engine, stdout());
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
                eprintln!("{err}");
                return;
            }
        };
        if server.command(&line) {
            return;
        }
        println!("ok");
    }
}

// Parse hh:mm:ss as a duration.
pub fn parse_hhmmss(time: &str) -> Option<Duration> {
    let mut toks = time.split(':');
    let hours = toks.next().unwrap_or("").parse::<u64>().ok()?;
    let minutes = toks.next().unwrap_or("").parse::<u64>().ok()?;
    let seconds = toks.next().unwrap_or("").parse::<u64>().ok()?;
    if toks.next().is_some() {
        return None;
    }
    Some(Duration::from_secs(hours * 3600 + minutes * 60 + seconds))
}

// Parse nnn[.nnn] as number of seconds.
pub fn parse_seconds(time: &str) -> Option<Duration> {
    let dot = time.find('.');
    let secs = time[..dot.unwrap_or(time.len())].parse::<u64>().ok()?;
    let mut dur = Duration::from_secs(secs);
    if let Some(i) = dot {
        let trailing = &time[i + 1..];
        let mut len = trailing.len();
        if len > 9 {
            return None;
        }
        let mut subsecs = trailing.parse::<u64>().ok()?;
        while len < 9 {
            len += 1;
            subsecs *= 10;
        }
        dur += Duration::from_nanos(subsecs);
    }
    Some(dur)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    struct DummyEngine;
    impl UhpEngine for DummyEngine {
        fn name(&self) -> &str {
            "dummy"
        }
        fn best_move(&mut self, _game: &GameState, _limit: BestMoveLimit) -> Turn {
            panic!("should not be called for game over")
        }
    }

    fn test_server() -> UhpServer<DummyEngine, Cursor<Vec<u8>>> {
        UhpServer::new(DummyEngine, Cursor::new(Vec::new()))
    }

    fn command<E: UhpEngine>(server: &mut UhpServer<E, Cursor<Vec<u8>>>, input: &str) -> String {
        server.swap_output(Cursor::new(Vec::new()));
        server.command(input);
        let buf = server.swap_output(Cursor::new(Vec::new()));
        String::from_utf8(buf.into_inner()).unwrap().trim().to_string()
    }

    #[test]
    fn test_parse_hhmmss() {
        assert_eq!(Some(Duration::from_secs(7)), parse_hhmmss("00:00:07"));
        assert_eq!(Some(Duration::from_secs(3661)), parse_hhmmss("01:01:01"));
        assert_eq!(None, parse_hhmmss("45"));
        assert_eq!(None, parse_hhmmss("1:23"));
        assert_eq!(None, parse_hhmmss("01:02:03:04"));
        assert_eq!(None, parse_hhmmss("five"));
    }

    #[test]
    fn test_parse_seconds() {
        assert_eq!(Some(Duration::from_millis(5)), parse_seconds("0.005"));
        assert_eq!(Some(Duration::from_secs(60)), parse_seconds("60"));
        assert_eq!(None, parse_seconds("01:23:45"));
        assert_eq!(None, parse_seconds("2e1"));
    }

    #[test]
    fn bestmove_on_finished_game_returns_error_instead_of_panicking() {
        let mut server = test_server();
        // Six-move-repetition draw position where Rules::get_winner() / game.get_winner() is Some(Draw).
        command(
            &mut server,
            r"newgame Base+ML;White[10];wB1;bB1 wB1-;wQ /wB1;bQ bB1\;wA1 -wB1;bQ /bB1;wA1 -wQ;bB1 bQ;wA1 /wQ;bB1 wB1;wA1 -bB1;bB1 bQ/;wA1 -wQ;bB1 bQ;wA1 /wQ;bB1 wB1;wA1 -bB1;bB1 bQ/",
        );

        assert!(command(&mut server, "bestmove depth 1").contains("err"));
    }
}
