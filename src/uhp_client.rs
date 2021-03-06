use crate::player::Player;
use crate::uhp_util::{Result, UhpBoard, UhpError};

use std::io::{BufRead, BufReader, Write};
use std::ops::Drop;
use std::path::Path;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::time::Duration;

pub(crate) struct UhpClient {
    proc: Child,
    input: ChildStdin,
    output: BufReader<ChildStdout>,
    board: UhpBoard,
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
        let mut client = UhpClient { proc, input, output, board: UhpBoard::new("Base") };
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
        self.board = UhpBoard::new(game_type);
        Ok(())
    }

    pub(crate) fn apply(&mut self, m: crate::Move) -> Result<()> {
        let mut command = "play ".to_owned();
        command.push_str(&self.board.to_move_string(m));
        let out = self.command(&command)?.join("\n");
        if out.starts_with("invalid") {
            return Err(UhpError::InvalidMove(command + ": " + &out));
        }
        self.board.apply(m)?;
        Ok(())
    }

    pub(crate) fn undo(&mut self, num_undo: usize) -> Result<()> {
        self.command(&format!("undo {}", num_undo))?;
        for _ in 0..num_undo {
            self.board.undo()?;
        }
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
}

impl UhpPlayer {
    pub(crate) fn new(cmd: &str) -> Result<Self> {
        Ok(UhpPlayer {
            client: UhpClient::new(&[cmd.to_owned()])?,
            cmd: Path::new(cmd).file_name().unwrap().to_str().unwrap().to_string(),
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

    fn generate_move(&mut self) -> crate::Move {
        self.client.best_move(Duration::from_secs(5)).unwrap()
    }
}
