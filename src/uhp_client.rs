extern crate minimax;

use crate::player::Player;
use crate::uhp_util::{Result, UhpError};
use crate::{Board, Color};

use minimax::Winner;
use std::io::{BufRead, BufReader, Write};
use std::ops::Drop;
use std::path::Path;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::time::Duration;

pub(crate) struct UhpClient {
    proc: Child,
    input: ChildStdin,
    output: BufReader<ChildStdout>,
    board: Board,
}

impl UhpClient {
    pub(crate) fn new(cmd_args: &[String]) -> Result<UhpClient> {
        let mut proc = Command::new(&cmd_args[0])
            .args(&cmd_args[1..])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        let input = proc.stdin.take().unwrap();
        let output = BufReader::new(proc.stdout.take().unwrap());
        let mut client = UhpClient { proc, input, output, board: Board::new_core_set() };
        // Eat the first output
        client.consume_output()?;
        Ok(client)
    }

    fn consume_output(&mut self) -> Result<Vec<String>> {
        let mut out = Vec::new();
        loop {
            let mut line = String::new();
            self.output.read_line(&mut line)?;
            if line.trim() == "ok" {
                return Ok(out);
            }
            out.push(line.trim().to_string());
            if line.starts_with("err") {
                return Err(UhpError::EngineError(out.join("\n")));
            }
        }
    }

    fn command(&mut self, command: &str) -> Result<Vec<String>> {
        let mut line = command.to_owned();
        line.push('\n');
        self.input.write_all(line.as_bytes())?;
        self.consume_output()
    }

    pub(crate) fn new_game(&mut self, game_type: &str) -> Result<()> {
        let mut command = "newgame ".to_owned();
        command.push_str(game_type);
        self.command(&command)?;
        self.board = Board::from_game_string(game_type)?;
        Ok(())
    }

    pub(crate) fn apply(&mut self, m: crate::Move) -> Result<Option<Winner>> {
        let mut command = "play ".to_owned();
        command.push_str(&self.board.to_move_string(m));
        let out = self.command(&command)?.join("\n");
        if out.starts_with("invalid") {
            return Err(UhpError::InvalidMove(command + ": " + &out));
        }
        self.board.apply_untrusted(m)?;
        Ok(match out.split(";").nth(1).unwrap_or_default() {
            "Draw" => Some(Winner::Draw),
            "WhiteWins" => Some(if self.board.to_move() == Color::White {
                Winner::PlayerToMove
            } else {
                Winner::PlayerJustMoved
            }),
            "BlackWins" => Some(if self.board.to_move() == Color::Black {
                Winner::PlayerToMove
            } else {
                Winner::PlayerJustMoved
            }),
            _ => None,
        })
    }

    pub(crate) fn undo(&mut self, num_undo: usize) -> Result<()> {
        self.command(&format!("undo {}", num_undo))?;
        self.board.undo_count(num_undo)?;
        Ok(())
    }

    pub(crate) fn raw_generate_moves(&mut self) -> Result<String> {
        Ok(self.command("validmoves")?[0].clone())
    }

    // Ask the engine for the next possible moves.
    pub(crate) fn generate_moves(&mut self) -> Result<Vec<crate::Move>> {
        let mut moves = Vec::new();
        for move_string in self.raw_generate_moves()?.split(';') {
            moves.push(self.board.from_move_string(move_string)?);
        }
        Ok(moves)
    }

    pub(crate) fn game_log(&mut self) -> String {
        self.board.game_log()
    }

    pub(crate) fn best_move(&mut self, timeout: Duration) -> Result<crate::Move> {
        let secs = timeout.as_secs();
        let h = secs / 3600;
        let m = secs % 3600 / 60;
        let s = secs % 60;
        let move_string =
            self.command(&format!("bestmove time {:02}:{:02}:{:02}", h, m, s))?.pop().unwrap();
        self.board.from_move_string(&move_string)
    }

    pub(crate) fn best_move_depth(&mut self, depth: u8) -> Result<crate::Move> {
        let move_string = self.command(&format!("bestmove depth {}", depth))?.pop().unwrap();
        self.board.from_move_string(&move_string)
    }
}

impl Drop for UhpClient {
    fn drop(&mut self) {
        if let Err(err) = self.proc.kill() {
            println!("{}", err);
        }
    }
}

pub(crate) struct UhpPlayer {
    client: UhpClient,
    cmd: String,
    timeout: Option<Duration>,
    depth: Option<u8>,
}

impl UhpPlayer {
    pub(crate) fn new(cmd: &str) -> Result<Self> {
        Ok(UhpPlayer {
            client: UhpClient::new(&[cmd.to_owned()])?,
            cmd: Path::new(cmd).file_name().unwrap().to_str().unwrap().to_string(),
            timeout: None,
            depth: None,
        })
    }
}

impl Player for UhpPlayer {
    fn name(&self) -> String {
        self.cmd.clone()
    }

    fn new_game(&mut self, game_type: &str) {
        self.client.new_game(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        self.client.apply(m).unwrap();
    }

    fn undo_move(&mut self, _: crate::Move) {
        self.client.undo(1).unwrap();
    }

    fn generate_move(&mut self) -> crate::Move {
        if let Some(depth) = self.depth {
            self.client.best_move_depth(depth).unwrap()
        } else {
            self.client.best_move(self.timeout.unwrap_or(Duration::from_secs(5))).unwrap()
        }
    }

    fn set_max_depth(&mut self, depth: u8) {
        self.depth = Some(depth);
    }

    fn set_timeout(&mut self, time: Duration) {
        self.timeout = Some(time);
    }
}
