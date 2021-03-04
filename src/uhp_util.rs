extern crate minimax;
use crate::{Board, Bug, Color, Id, Rules, UNASSIGNED};
use minimax::{Game, Move};
use std::collections::HashMap;

pub(crate) struct UhpBoard {
    board: Board,
    // Maps "wA1" to its location or UNASSIGNED if not placed yet.
    name_to_id: HashMap<String, Id>,
    // Reverse map for pieces in play.
    id_to_name_stack: HashMap<Id, Vec<String>>,
    game_type: String,
    move_history: Vec<crate::Move>,
}

fn bug_name(color: Color, bug: Bug, number: u8) -> String {
    let mut name = match color {
        // Standard seems to be white goes first.
        Color::Black => "b",
        Color::White => "w",
    }
    .to_string();
    name.push(bug.name().chars().next().unwrap().to_ascii_uppercase());
    if bug == Bug::Ant || bug == Bug::Spider || bug == Bug::Grasshopper || bug == Bug::Beetle {
        name.push((number + 48) as u8 as char);
    }
    name
}

#[derive(Debug)]
pub(crate) enum UhpError {
    IoError(std::io::Error),
    UnknownPiece(String),
    InvalidGameString(String),
    InvalidMove(String),
    GameNotStarted,
    UnrecognizedCommand(String),
    TooManyUndos,
}

impl From<std::io::Error> for UhpError {
    fn from(error: std::io::Error) -> Self {
        UhpError::IoError(error)
    }
}

pub(crate) type Result<T> = std::result::Result<T, UhpError>;

impl UhpBoard {
    pub(crate) fn new(game_type: &str) -> Self {
        // Generate names of all pieces.
        let mut name_to_id = HashMap::new();
        let board = Board::new_from_game_type(game_type).unwrap();
        for &color in &[Color::Black, Color::White] {
            for (bug, num_bugs) in board.get_available_bugs().iter() {
                for num in 0..*num_bugs {
                    name_to_id.insert(bug_name(color, *bug, num + 1), UNASSIGNED);
                }
            }
        }
        UhpBoard {
            board,
            name_to_id,
            id_to_name_stack: HashMap::new(),
            game_type: game_type.to_string(),
            move_history: Vec::new(),
        }
    }

    pub(crate) fn to_id(&self, piece_string: &str) -> Result<Id> {
        if let Some(id) = self.name_to_id.get(piece_string) {
            return Ok(*id);
        }
        Ok(if "\\-/".contains(&piece_string[..1]) {
            let name = &piece_string[1..];
            let neighbors = self.board.adjacent(self.name_to_id[name]);
            match &piece_string[..1] {
                "\\" => neighbors[0],
                "-" => neighbors[5],
                "/" => neighbors[4],
                _ => return Err(UhpError::UnknownPiece(piece_string.to_owned())),
            }
        } else if "\\-/".contains(&piece_string[piece_string.len() - 1..]) {
            let name = &piece_string[..piece_string.len() - 1];
            let neighbors = self.board.adjacent(self.name_to_id[name]);
            match &piece_string[piece_string.len() - 1..] {
                "/" => neighbors[1],
                "-" => neighbors[2],
                "\\" => neighbors[3],
                _ => return Err(UhpError::UnknownPiece(piece_string.to_owned())),
            }
        } else {
            return Err(UhpError::UnknownPiece(piece_string.to_owned()));
        })
    }

    // https://github.com/jonthysell/Mzinga/wiki/UniversalHiveProtocol#movestring
    pub(crate) fn from_move_string(&self, move_string: &str) -> Result<crate::Move> {
        if move_string == "pass" {
            return Ok(crate::Move::Pass);
        }
        let tokens = move_string.split(' ').collect::<Vec<_>>();
        let start: Id = self.to_id(tokens[0])?;
        let bug = Bug::from_char(tokens[0].chars().nth(1).unwrap()).unwrap();
        if self.move_history.is_empty() {
            if tokens.len() != 1 {
                return Err(UhpError::InvalidMove(move_string.to_owned()));
            }
            return Ok(crate::Move::Place((0, 0), bug)); // Place at origin.
        }
        if tokens.len() != 2 {
            return Err(UhpError::InvalidMove(move_string.to_owned()));
        }
        let end: Id = self.to_id(tokens[1])?;
        Ok(if start == UNASSIGNED {
            crate::Move::Place(self.board.loc(end), bug)
        } else {
            crate::Move::Movement(self.board.loc(start), self.board.loc(end))
        })
    }

    fn next_bug_num(&self, bug: Bug) -> u8 {
        [1, 3, 2, 3, 2, 1, 1, 1][bug as usize] - self.board.get_remaining()[bug as usize] + 1
    }

    pub(crate) fn to_move_string(&self, m: crate::Move) -> String {
        let mut move_string = match m {
            crate::Move::Place(_, bug) => {
                bug_name(self.board.to_move(), bug, self.next_bug_num(bug))
            }
            crate::Move::Movement(start, _) => {
                let id = self.board.id(start);
                self.id_to_name_stack.get(&id).unwrap().last().unwrap().clone()
            }
            crate::Move::Pass => return "pass".to_owned(),
        };
        if self.move_history.is_empty() {
            return move_string;
        }
        move_string.push(' ');

        let end = self.board.id(match m {
            crate::Move::Place(loc, _) => loc,
            crate::Move::Movement(_, end) => end,
            crate::Move::Pass => unreachable!(),
        });
        if let Some(name) = self.id_to_name_stack.get(&end).map(|stack| stack.last()).flatten() {
            move_string.push_str(name);
            return move_string;
        }
        for (dir, adj) in self.board.adjacent(end).iter().enumerate() {
            if self.board.get(*adj).is_some() {
                let name = self.id_to_name_stack.get(adj).unwrap().last().unwrap().clone();
                // Reverse directions; they're from the other bug's perspective.
                move_string.push_str(match dir {
                    3 => "\\",
                    2 => "-",
                    1 => "/",
                    _ => "",
                });
                move_string.push_str(&name);
                move_string.push_str(match dir {
                    4 => "/",
                    5 => "-",
                    0 => "\\",
                    _ => "",
                });
                return move_string;
            }
        }
        unreachable!("no neighboring pieces")
    }

    pub(crate) fn apply_untrusted(&mut self, m: crate::Move) -> Result<()> {
        let mut moves = Vec::new();
        Rules::generate_moves(&self.board, &mut moves);
        if !moves.contains(&m) {
            return Err(UhpError::InvalidMove("That is not a valid move".to_string()));
        }
        self.apply(m)
    }

    pub(crate) fn apply(&mut self, m: crate::Move) -> Result<()> {
        match m {
            crate::Move::Place(loc, bug) => {
                let id = self.board.id(loc);
                let color = self.board.to_move();
                let name = bug_name(color, bug, self.next_bug_num(bug));
                self.name_to_id.insert(name.clone(), id);
                self.id_to_name_stack.insert(id, vec![name]);
            }
            crate::Move::Movement(start_loc, end_loc) => {
                let start = self.board.id(start_loc);
                let end = self.board.id(end_loc);
                let name = self.id_to_name_stack.get_mut(&start).unwrap().pop().unwrap();
                self.name_to_id.insert(name.clone(), end);
                self.id_to_name_stack.entry(end).or_insert_with(Vec::new).push(name);
            }
            crate::Move::Pass => {}
        }
        m.apply(&mut self.board);
        self.move_history.push(m);
        Ok(())
    }

    pub(crate) fn undo(&mut self) -> Result<()> {
        let m = self.move_history.pop().ok_or(UhpError::TooManyUndos)?;
        match m {
            crate::Move::Place(loc, _) => {
                let id = self.board.id(loc);
                let name = self.id_to_name_stack.get_mut(&id).unwrap().pop().unwrap();
                self.name_to_id.insert(name, UNASSIGNED);
            }
            crate::Move::Movement(start_loc, end_loc) => {
                let start = self.board.id(start_loc);
                let end = self.board.id(end_loc);
                let name = self.id_to_name_stack.get_mut(&end).unwrap().pop().unwrap();
                self.name_to_id.insert(name.clone(), start);
                self.id_to_name_stack.entry(start).or_insert_with(Vec::new).push(name);
            }
            crate::Move::Pass => {}
        }
        m.undo(&mut self.board);
        Ok(())
    }

    pub(crate) fn from_game_string(s: &str) -> Result<Self> {
        let mut toks = s.split(';');
        let game_type = toks.next().ok_or_else(|| UhpError::InvalidGameString(s.to_owned()))?;
        let mut board = UhpBoard::new(game_type);
        // We don't actually care about the game state, but
        // we'll just treat this like a game type if it's
        if toks.next().is_none() {
            return Ok(board);
        }
        // Don't care about turn string either, although it would say which
        // bug moved first?
        toks.next().ok_or_else(|| UhpError::InvalidGameString(s.to_owned()))?;
        // The rest are move strings.
        for move_string in toks {
            let m = board.from_move_string(move_string)?;
            board.apply(m)?;
        }
        Ok(board)
    }

    pub(crate) fn into_inner(self) -> Board {
        self.board
    }

    pub(crate) fn inner(&self) -> &Board {
        &self.board
    }

    pub(crate) fn valid_moves(&self) -> String {
        let mut moves = Vec::new();
        Rules::generate_moves(&self.board, &mut moves);
        let mut out = String::new();
        for m in moves {
            out.push_str(&self.to_move_string(m));
            out.push(';');
        }
        if out.ends_with(';') {
            out.pop();
        }
        out
    }

    pub(crate) fn game_log(&mut self) -> String {
        let history = self.move_history.clone();
        for _ in 0..history.len() {
            self.undo().unwrap();
        }
        let mut log = String::new();
        for &m in history.iter() {
            log.push_str(&self.to_move_string(m));
            log.push(';');
            self.apply(m).unwrap();
        }
        if log.ends_with(';') {
            log.pop();
        }
        log
    }

    fn game_state_string(&self) -> &'static str {
        if self.move_history.is_empty() {
            return "NotStarted";
        }
        match Rules::get_winner(&self.board) {
            Some(minimax::Winner::Draw) => "Draw",
            Some(minimax::Winner::PlayerToMove) => match self.board.to_move() {
                Color::Black => "BlackWins",
                Color::White => "WhiteWins",
            },
            Some(minimax::Winner::PlayerJustMoved) => match self.board.to_move() {
                Color::White => "BlackWins",
                Color::Black => "WhiteWins",
            },
            None => "InProgress",
        }
    }

    fn turn_string(&self) -> String {
        format!("{:?}[{}]", self.board.to_move(), self.move_history.len() / 2 + 1)
    }

    pub(crate) fn game_string(&mut self) -> String {
        let mut out = self.game_type.clone();
        out.push(';');
        out.push_str(self.game_state_string());
        out.push(';');
        out.push_str(&self.turn_string());
        if !self.move_history.is_empty() {
            out.push(';');
            out.push_str(&self.game_log());
        }
        out
    }
}

#[cfg(test)]
mod tests {
    extern crate rand;
    use super::*;
    use crate::Rules;
    use minimax::Game;
    use rand::Rng;

    #[test]
    fn test_move_string_round_trip() {
        let mut b = UhpBoard::new("Base+MLP");
        let mut rng = rand::thread_rng();
        for iter in 0..20 {
            let mut moves = Vec::new();
            let mut depth = 0;
            for _ in 0..20 {
                depth += 1;
                moves.clear();
                Rules::generate_moves(&b.board, &mut moves);
                let m = moves[rng.gen_range(0, moves.len())];
                let move_string = b.to_move_string(m);
                assert_eq!(
                    m,
                    b.from_move_string(&move_string).unwrap(),
                    "iter={}, move_string={}",
                    iter,
                    move_string,
                );
                b.apply(m).unwrap();
                if Rules::get_winner(&b.board).is_some() {
                    break;
                }
            }
            for _ in 0..depth {
                b.undo().unwrap();
            }
        }
    }
}
