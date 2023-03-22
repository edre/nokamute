extern crate git_version;
extern crate minimax;

use crate::notation::{Result, UhpError};
use crate::*;

use minimax::*;
use std::io::Write;
#[cfg(not(target_arch = "wasm32"))]
use std::io::{stdin, stdout};
use std::time::Duration;

pub struct UhpServer<W: Write> {
    board: Option<Board>,
    pv_dirty: bool,
    config: PlayerConfig,
    engine: Option<Box<dyn Player>>,
    output: W,
}

impl<W: Write> UhpServer<W> {
    pub fn new(config: PlayerConfig, output: W) -> Self {
        UhpServer { board: None, pv_dirty: true, config, engine: None, output }
    }

    pub fn swap_output(&mut self, mut output: W) -> W {
        std::mem::swap(&mut output, &mut self.output);
        output
    }

    fn info(&mut self) -> Result<()> {
        // Version string
        let mut version =
            git_version::git_version!(args = ["--tags", "--dirty=+"], cargo_prefix = "cargo-");
        if let Some(stripped) = version.strip_prefix('v') {
            version = stripped;
        }
        writeln!(self.output, "id {} {}", env!("CARGO_PKG_NAME"), version)?;
        // Capabilities
        writeln!(self.output, "Mosquito;Ladybug;Pillbug")?;
        Ok(())
    }

    fn reset_engine(&mut self) {
        if let Some(board) = &self.board {
            let mut engine = self.config.new_player();
            engine.new_game(&board.game_type());
            for &turn in &board.turn_history {
                engine.play_move(turn);
            }
            self.engine = Some(engine);
        }
    }

    fn new_game(&mut self, args: &str) -> Result<()> {
        self.pv_dirty = true;
        let args = if args.is_empty() { "Base" } else { args };
        self.board = Some(Board::from_game_string(args)?);
        self.reset_engine();
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
        self.pv_dirty = true;
        let board = self.board.as_mut().ok_or(UhpError::GameNotStarted)?;
        let m = board.from_move_string(args)?;
        board.apply_untrusted(m)?;
        self.engine.as_mut().unwrap().play_move(m);
        writeln!(self.output, "{}", board.game_string())?;
        Ok(())
    }

    fn best_move(&mut self, args: &str) -> Result<()> {
        self.pv_dirty = false;
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

    fn pv(&mut self) -> Result<()> {
        let pv = self.engine.as_ref().ok_or(UhpError::GameNotStarted)?.principal_variation();
        let board = self.board.as_mut().unwrap();
        if self.pv_dirty {
            return Err(UhpError::EngineError("Board changed since last engine move".into()));
        }
        for &m in &pv {
            writeln!(self.output, "{}", board.to_move_string(m))?;
            board.apply(m);
        }
        for &m in pv.iter().rev() {
            board.undo(m);
        }
        Ok(())
    }

    fn undo(&mut self, args: &str) -> Result<()> {
        self.pv_dirty = true;
        let board = self.board.as_mut().ok_or(UhpError::GameNotStarted)?;
        let num_undo = if args.is_empty() {
            1
        } else {
            args.parse::<usize>().map_err(|_| UhpError::UnrecognizedCommand(args.to_string()))?
        };
        if num_undo > board.turn_history.len() {
            return Err(UhpError::TooManyUndos);
        }
        for _ in 0..num_undo {
            self.engine.as_mut().unwrap().undo_move(board.last_move().unwrap());
            board.undo_count(1)?;
        }
        writeln!(self.output, "{}", board.game_string())?;
        Ok(())
    }

    fn get_option_int<Option: UhpOptionInt>(&mut self) -> Result<()> {
        writeln!(
            self.output,
            "{};int;{};{};{};{}",
            Option::name(),
            Option::current(&self.config)?,
            Option::current(&PlayerConfig::default())?,
            Option::min(),
            Option::max()
        )?;
        Ok(())
    }

    fn get_option_bool<Option: UhpOptionBool>(&mut self) -> Result<()> {
        fn fmt_bool(b: bool) -> &'static str {
            if b {
                "True"
            } else {
                "False"
            }
        }
        writeln!(
            self.output,
            "{};bool;{};{}",
            Option::name(),
            fmt_bool(Option::current(&self.config)?),
            fmt_bool(Option::current(&PlayerConfig::default())?)
        )?;
        Ok(())
    }

    fn set_option_int<Option: UhpOptionInt>(&mut self, arg: &str) -> Result<()> {
        let value = arg.parse::<usize>().map_err(|_| UhpError::InvalidOption(arg.into()))?;
        if value < Option::min() || value > Option::max() {
            return Err(UhpError::InvalidOption(arg.into()));
        }
        Option::set(value, &mut self.config);
        self.get_option_int::<Option>()
    }

    fn set_option_bool<Option: UhpOptionBool>(&mut self, arg: &str) -> Result<()> {
        let value = match arg {
            "True" => true,
            "False" => false,
            _ => return Err(UhpError::InvalidOption(arg.into())),
        };
        Option::set(value, &mut self.config);
        self.get_option_bool::<Option>()
    }

    fn get_option(&mut self, option: &str) -> Result<()> {
        match option {
            "Aggression" => self.get_option_int::<AggressionOption>(),
            #[cfg(not(target_arch = "wasm32"))]
            "BackgroundPondering" => self.get_option_bool::<BackgroundPonderingOption>(),
            #[cfg(not(target_arch = "wasm32"))]
            "NumThreads" => self.get_option_int::<NumThreadsOption>(),
            "RandomOpening" => self.get_option_bool::<RandomOpeningOption>(),
            "TableSizeMiB" => self.get_option_int::<TableSizeOption>(),
            "Verbose" => self.get_option_bool::<VerboseOption>(),
            _ => Err(UhpError::InvalidOption(option.into())),
        }
    }

    fn options(&mut self, args: &str) -> Result<()> {
        let tokens = args.split(' ').collect::<Vec<_>>();
        if args.is_empty() {
            self.get_option_int::<AggressionOption>()?;
            #[cfg(not(target_arch = "wasm32"))]
            self.get_option_bool::<BackgroundPonderingOption>()?;
            #[cfg(not(target_arch = "wasm32"))]
            self.get_option_int::<NumThreadsOption>()?;
            self.get_option_bool::<RandomOpeningOption>()?;
            self.get_option_int::<TableSizeOption>()?;
            self.get_option_bool::<VerboseOption>()?;
        } else if tokens.len() == 2 && tokens[0] == "get" {
            self.get_option(tokens[1])?;
        } else if tokens.len() == 3 && tokens[0] == "set" {
            match tokens[1] {
                "Aggression" => self.set_option_int::<AggressionOption>(tokens[2])?,
                #[cfg(not(target_arch = "wasm32"))]
                "BackgroundPondering" => {
                    self.set_option_bool::<BackgroundPonderingOption>(tokens[2])?
                }
                #[cfg(not(target_arch = "wasm32"))]
                "NumThreads" => self.set_option_int::<NumThreadsOption>(tokens[2])?,
                "RandomOpening" => self.set_option_bool::<RandomOpeningOption>(tokens[2])?,
                "TableSizeMiB" => self.set_option_int::<TableSizeOption>(tokens[2])?,
                "Verbose" => self.set_option_bool::<VerboseOption>(tokens[2])?,
                _ => return Err(UhpError::InvalidOption(args.into())),
            }
            self.reset_engine();
        } else {
            return Err(UhpError::UnrecognizedCommand(args.into()));
        }
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
            "pv" => self.pv(),
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

#[cfg(not(target_arch = "wasm32"))]
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

trait UhpOptionInt {
    fn name() -> &'static str;
    fn current(config: &PlayerConfig) -> Result<usize>;
    fn min() -> usize;
    fn max() -> usize;
    // Caller does bounds checking.
    fn set(value: usize, config: &mut PlayerConfig);
}

#[cfg(not(target_arch = "wasm32"))]
struct NumThreadsOption {}
#[cfg(not(target_arch = "wasm32"))]
impl UhpOptionInt for NumThreadsOption {
    fn name() -> &'static str {
        "NumThreads"
    }
    fn current(config: &PlayerConfig) -> Result<usize> {
        Ok(if let Some(num_threads) = config.num_threads { num_threads } else { Self::max() })
    }
    fn min() -> usize {
        1
    }
    fn max() -> usize {
        ParallelOptions::default().num_threads()
    }
    fn set(value: usize, config: &mut PlayerConfig) {
        config.num_threads = Some(value)
    }
}

struct AggressionOption {}
impl UhpOptionInt for AggressionOption {
    fn name() -> &'static str {
        "Aggression"
    }
    fn current(config: &PlayerConfig) -> Result<usize> {
        Ok(config.eval.aggression().into())
    }
    fn min() -> usize {
        1
    }
    fn max() -> usize {
        5
    }
    fn set(value: usize, config: &mut PlayerConfig) {
        config.eval = BasicEvaluator::new(value as u8);
    }
}

struct TableSizeOption {}
impl UhpOptionInt for TableSizeOption {
    fn name() -> &'static str {
        "TableSizeMiB"
    }
    fn current(config: &PlayerConfig) -> Result<usize> {
        Ok(config.opts.table_byte_size >> 20)
    }
    fn min() -> usize {
        1
    }
    fn max() -> usize {
        256
    }
    fn set(value: usize, config: &mut PlayerConfig) {
        config.opts.table_byte_size = value << 20
    }
}

trait UhpOptionBool {
    fn name() -> &'static str;
    fn current(config: &PlayerConfig) -> Result<bool>;
    fn set(value: bool, config: &mut PlayerConfig);
}

struct VerboseOption {}
impl UhpOptionBool for VerboseOption {
    fn name() -> &'static str {
        "Verbose"
    }
    fn current(config: &PlayerConfig) -> Result<bool> {
        Ok(config.opts.verbose)
    }
    fn set(value: bool, config: &mut PlayerConfig) {
        config.opts.verbose = value;
    }
}

struct RandomOpeningOption {}
impl UhpOptionBool for RandomOpeningOption {
    fn name() -> &'static str {
        "RandomOpening"
    }
    fn current(config: &PlayerConfig) -> Result<bool> {
        Ok(config.random_opening)
    }
    fn set(value: bool, config: &mut PlayerConfig) {
        config.random_opening = value;
    }
}

#[cfg(not(target_arch = "wasm32"))]
struct BackgroundPonderingOption {}
#[cfg(not(target_arch = "wasm32"))]
impl UhpOptionBool for BackgroundPonderingOption {
    fn name() -> &'static str {
        "BackgroundPondering"
    }
    fn current(config: &PlayerConfig) -> Result<bool> {
        let parallel_opts = if let PlayerStrategy::Iterative(parallel_opts) = config.strategy {
            parallel_opts
        } else {
            return Err(UhpError::EngineError("Unexpected config".into()));
        };
        Ok(parallel_opts.background_pondering)
    }
    fn set(value: bool, config: &mut PlayerConfig) {
        if let PlayerStrategy::Iterative(ref mut parallel_opts) = config.strategy {
            parallel_opts.background_pondering = value;
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
