use crate::{Color, GameState, Result, Turn, UhpError, Winner};

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Write};
use std::ops::Drop;
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::time::Duration;

pub struct UhpClient {
    proc: Child,
    input: ChildStdin,
    output: BufReader<ChildStdout>,
    game: GameState,
    pub name: String,
    pub capabilities: String,
}

impl UhpClient {
    pub fn new(cmd_args: &[String]) -> Result<UhpClient> {
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
            game: GameState::new_core_set(),
            name: String::new(),
            capabilities: String::new(),
        };
        let id = client.consume_output()?;
        client.name = id
            .first()
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

    pub fn new_game(&mut self, game_type: &str) -> Result<String> {
        let mut command = "newgame ".to_owned();
        command.push_str(game_type);
        let output = self.command(&command)?.join("\n");
        self.game = GameState::from_game_string(game_type)?;
        Ok(output)
    }

    pub fn raw_play(&mut self, move_string: &str) -> Result<String> {
        let command = format!("play {move_string}");
        let out = self.command(&command)?.join("\n");
        self.game.apply_untrusted(self.game.from_move_string(move_string)?)?;
        Ok(out)
    }

    pub fn apply(&mut self, m: Turn) -> Result<Option<Winner>> {
        let command = format!("play {}", self.game.to_move_string(m));
        let out = self.command(&command)?.join("\n");
        self.game.apply_untrusted(m)?;
        Ok(match out.split(';').nth(1).unwrap_or_default() {
            "Draw" => Some(Winner::Draw),
            "WhiteWins" => Some(if self.game.to_move() == Color::White {
                Winner::PlayerToMove
            } else {
                Winner::PlayerJustMoved
            }),
            "BlackWins" => Some(if self.game.to_move() == Color::Black {
                Winner::PlayerToMove
            } else {
                Winner::PlayerJustMoved
            }),
            _ => None,
        })
    }

    pub fn undo(&mut self, num_undo: usize) -> Result<()> {
        self.command(&format!("undo {num_undo}"))?;
        self.game.undo_count(num_undo)?;
        Ok(())
    }

    pub fn raw_generate_moves(&mut self) -> Result<String> {
        Ok(self.command("validmoves")?[0].clone())
    }

    // Ask the engine for the next possible moves.
    pub fn generate_moves(&mut self) -> Result<Vec<Turn>> {
        let mut moves = Vec::new();
        for move_string in self.raw_generate_moves()?.split(';') {
            moves.push(self.game.from_move_string(move_string)?);
        }
        Ok(moves)
    }

    pub fn game_log(&mut self) -> String {
        self.game.game_log()
    }

    pub fn best_move(&mut self, timeout: Duration) -> Result<Turn> {
        let secs = timeout.as_secs();
        let h = secs / 3600;
        let m = secs % 3600 / 60;
        let s = secs % 60;
        let move_string =
            self.command(&format!("bestmove time {h:02}:{m:02}:{s:02}"))?.pop().unwrap();
        self.game.from_move_string(&move_string)
    }

    pub fn best_move_depth(&mut self, depth: u8) -> Result<Turn> {
        let move_string = self.command(&format!("bestmove depth {depth}"))?.pop().unwrap();
        self.game.from_move_string(&move_string)
    }
}

impl Drop for UhpClient {
    fn drop(&mut self) {
        if let Err(err) = self.proc.kill() {
            println!("{err}");
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct MoveDifference<'a> {
    pub missing: Vec<&'a str>,
    pub extra: Vec<&'a str>,
    pub invalid_expected: Vec<&'a str>,
    pub invalid_actual: Vec<&'a str>,
}

impl MoveDifference<'_> {
    pub fn is_empty(&self) -> bool {
        self.missing.is_empty()
            && self.extra.is_empty()
            && self.invalid_expected.is_empty()
            && self.invalid_actual.is_empty()
    }
}

pub fn semantic_move_diff<'a>(
    game: &GameState, expected: &[&'a str], actual: &[&'a str],
) -> MoveDifference<'a> {
    fn group_moves<'a>(
        game: &GameState, moves: &[&'a str],
    ) -> (BTreeMap<Turn, Vec<&'a str>>, Vec<&'a str>) {
        let mut grouped = BTreeMap::<Turn, Vec<&str>>::new();
        let mut invalid = Vec::new();
        for &move_string in moves {
            match parse_comparable_move(game, move_string) {
                Ok(turn) => grouped.entry(turn).or_default().push(move_string),
                Err(_) => invalid.push(move_string),
            }
        }
        (grouped, invalid)
    }

    // Keep each original string so diagnostics reproduce the exact notation after matching turns.
    let (expected_by_turn, invalid_expected) = group_moves(game, expected);
    let (mut actual_by_turn, invalid_actual) = group_moves(game, actual);
    let mut difference = MoveDifference { invalid_expected, invalid_actual, ..Default::default() };

    for (turn, expected_strings) in expected_by_turn {
        let actual_strings = actual_by_turn.remove(&turn).unwrap_or_default();
        let matched = expected_strings.len().min(actual_strings.len());
        difference.missing.extend_from_slice(&expected_strings[matched..]);
        difference.extra.extend_from_slice(&actual_strings[matched..]);
    }
    for actual_strings in actual_by_turn.into_values() {
        difference.extra.extend(actual_strings);
    }

    difference
}

fn parse_comparable_move(
    game: &GameState, move_string: &str,
) -> Result<Turn> {
    let move_string = move_string.trim();
    let turn = game.from_move_string(move_string)?;
    if let Turn::Move(_, _) = turn {
        let source = move_string.split_once(' ').map(|(source, _)| source);
        let canonical = game.to_move_string(turn);
        let canonical_source = canonical.split_once(' ').map(|(source, _)| source);
        // Pillbug throws may move an opponent's piece, but the named source must still be on top.
        if source != canonical_source {
            return Err(UhpError::InvalidMove(move_string.to_owned()));
        }
    }
    Ok(turn)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_semantic_move_diff_accepts_equivalent_references() {
        let game = GameState::from_game_string("Base;InProgress;White[3];wG1;bG1 wG1-;wQ -wG1;bQ bG1-")
            .unwrap();
        let difference = semantic_move_diff(&game, &[r"wA1 \wG1"], &["wA1 wQ/"]);
        assert!(difference.is_empty(), "{difference:?}");
    }

    #[test]
    fn test_semantic_move_diff_finds_equal_count_mismatch() {
        let game = GameState::from_game_string("Base;InProgress;White[3];wG1;bG1 wG1-;wQ -wG1;bQ bG1-")
            .unwrap();
        let difference = semantic_move_diff(&game, &[r"wA1 \wG1"], &["wA1 -wQ"]);
        assert_eq!(difference.missing, vec![r"wA1 \wG1"]);
        assert_eq!(difference.extra, vec!["wA1 -wQ"]);
    }

    #[test]
    fn test_semantic_move_diff_finds_duplicates_and_invalid_moves() {
        let game = GameState::from_game_string("Base;InProgress;White[3];wG1;bG1 wG1-;wQ -wG1;bQ bG1-")
            .unwrap();
        let difference = semantic_move_diff(
            &game,
            &[r"wA1 \wG1", "invalid-fixture-move"],
            &[r"wA1 \wG1", "wA1 wQ/", "invalid-engine-move"],
        );
        assert_eq!(difference.extra, vec!["wA1 wQ/"]);
        assert_eq!(difference.invalid_expected, vec!["invalid-fixture-move"]);
        assert_eq!(difference.invalid_actual, vec!["invalid-engine-move"]);
    }

    #[test]
    fn test_semantic_move_diff_rejects_covered_sources() {
        let game = GameState::from_game_string(
            r"Base;InProgress;White[10];wG1;bG1 wG1-;wQ /wG1;bQ bG1-;wS1 wQ\;bA1 bQ-;wB1 /wS1;bA1 -wQ;wB1 wS1\;bA2 bQ-;wB1 /wS1;bA2 wG1\;wB1 wS1\;bA3 bQ-;wB1 /wS1;bS1 bQ\;wB1 wS1;bS1 wB1\",
        )
        .unwrap();
        let legal = "wB1 /wB1";
        let covered = "wS1 /wB1";
        assert_eq!(game.from_move_string(legal).unwrap(), game.from_move_string(covered).unwrap());

        let difference = semantic_move_diff(&game, &[legal], &[covered]);
        assert_eq!(difference.missing, vec![legal]);
        assert_eq!(difference.invalid_actual, vec![covered]);
    }
}
