extern crate minimax;

use crate::notation::{Result, UhpError};
use crate::{Board, Color, Player, Turn};

use minimax::Winner;
use std::io::{BufRead, BufReader, Write};
use std::ops::Drop;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::time::Duration;

pub(crate) struct UhpClient {
    proc: Child,
    input: ChildStdin,
    output: BufReader<ChildStdout>,
    board: Board,
    pub name: String,
    pub capabilities: String,
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
        let mut client = UhpClient {
            proc,
            input,
            output,
            board: Board::new_core_set(),
            name: String::new(),
            capabilities: String::new(),
        };
        let id = client.consume_output()?;
        client.name = id
            .get(0)
            .cloned()
            .unwrap_or_default()
            .strip_prefix("id ")
            .unwrap_or("somebot")
            .to_string();
        client.capabilities = id.get(1).cloned().unwrap_or_default();
        Ok(client)
    }

    pub fn capable_of_game_string(&self, game_string: &str) -> bool {
        let game_type = game_string.split(';').next().unwrap();
        if !game_type.starts_with("Base+") {
            return true;
        }
        game_type[5..].chars().all(|expansion| self.capabilities.contains(expansion))
    }

    fn consume_output(&mut self) -> Result<Vec<String>> {
        let mut out = Vec::new();
        let mut err = None;
        loop {
            let mut line = String::new();
            self.output.read_line(&mut line)?;
            if line.trim() == "ok" {
                break;
            }
            out.push(line.trim().to_string());
            if line.starts_with("err") || line.starts_with("invalidmove") {
                err = Some(UhpError::EngineError(out.join("\n")));
            }
        }
        match err {
            Some(error) => Err(error),
            None => Ok(out),
        }
    }

    fn command(&mut self, command: &str) -> Result<Vec<String>> {
        let mut line = command.to_owned();
        line.push('\n');
        self.input.write_all(line.as_bytes())?;
        self.consume_output()
    }

    pub(crate) fn new_game(&mut self, game_type: &str) -> Result<String> {
        let mut command = "newgame ".to_owned();
        command.push_str(game_type);
        let output = self.command(&command)?.join("\n");
        self.board = Board::from_game_string(game_type)?;
        Ok(output)
    }

    pub(crate) fn raw_play(&mut self, move_string: &str) -> Result<String> {
        let command = format!("play {}", move_string);
        let out = self.command(&command)?.join("\n");
        self.board.apply_untrusted(self.board.from_move_string(move_string)?)?;
        Ok(out)
    }

    pub(crate) fn apply(&mut self, m: Turn) -> Result<Option<Winner>> {
        let command = format!("play {}", self.board.to_move_string(m));
        let out = self.command(&command)?.join("\n");
        self.board.apply_untrusted(m)?;
        Ok(match out.split(';').nth(1).unwrap_or_default() {
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
    pub(crate) fn generate_moves(&mut self) -> Result<Vec<Turn>> {
        let mut moves = Vec::new();
        for move_string in self.raw_generate_moves()?.split(';') {
            moves.push(self.board.from_move_string(move_string)?);
        }
        Ok(moves)
    }

    pub(crate) fn game_log(&mut self) -> String {
        self.board.game_log()
    }

    pub(crate) fn best_move(&mut self, timeout: Duration) -> Result<Turn> {
        let secs = timeout.as_secs();
        let h = secs / 3600;
        let m = secs % 3600 / 60;
        let s = secs % 60;
        let move_string =
            self.command(&format!("bestmove time {:02}:{:02}:{:02}", h, m, s))?.pop().unwrap();
        self.board.from_move_string(&move_string)
    }

    pub(crate) fn best_move_depth(&mut self, depth: u8) -> Result<Turn> {
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
    timeout: Option<Duration>,
    depth: Option<u8>,
}

impl UhpPlayer {
    pub(crate) fn new(cmd: &str) -> Result<Self> {
        Ok(UhpPlayer { client: UhpClient::new(&[cmd.to_owned()])?, timeout: None, depth: None })
    }
}

impl Player for UhpPlayer {
    fn name(&self) -> String {
        self.client.name.clone()
    }

    fn new_game(&mut self, game_type: &str) {
        self.client.new_game(game_type).unwrap();
    }

    fn play_move(&mut self, m: Turn) {
        self.client.apply(m).unwrap();
    }

    fn undo_move(&mut self, _: Turn) {
        self.client.undo(1).unwrap();
    }

    fn generate_move(&mut self) -> Turn {
        if let Some(depth) = self.depth {
            self.client.best_move_depth(depth).unwrap()
        } else {
            self.client.best_move(self.timeout.unwrap_or_else(|| Duration::from_secs(5))).unwrap()
        }
    }

    fn set_max_depth(&mut self, depth: u8) {
        self.depth = Some(depth);
    }

    fn set_timeout(&mut self, time: Duration) {
        self.timeout = Some(time);
    }
}
