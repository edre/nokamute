use crate::{Board, Bug, Color, Id, UNASSIGNED};
use minimax::Move;
use std::collections::HashMap;

pub(crate) struct UhpBoard {
    board: Board,
    // Maps "wA1" to its location or UNASSIGNED if not placed yet.
    name_to_id: HashMap<String, Id>,
    // Reverse map for pieces in play.
    id_to_name_stack: HashMap<Id, Vec<String>>,
    move_history: Vec<crate::Move>,
    // Locations in UHP are a direction relative to existing pieces.
    // The placement of the second piece determines how our board is oriented
    // relative to the UHP board.
    // TODO: rotation_offset: usize,
}

fn bug_name(color: Color, bug: Bug, number: u8) -> String {
    let mut name = match color {
        // UHP says white goes first, I say black.
        Color::Black => "w",
        Color::White => "b",
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
    InvalidMove(String),
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
            board: board,
            name_to_id: name_to_id,
            id_to_name_stack: HashMap::new(),
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
        let tokens = move_string.split(' ').collect::<Vec<_>>();
        let start: Id = self.to_id(tokens[0])?;
        let bug = Bug::from_char(tokens[0].chars().nth(1).unwrap()).unwrap();
        if self.move_history.len() == 0 {
            if tokens.len() != 1 {
                return Err(UhpError::InvalidMove(move_string.to_owned()));
            }
            return Ok(crate::Move::Place(1, bug)); // Place at ORIGIN.
        }
        if tokens.len() != 2 {
            return Err(UhpError::InvalidMove(move_string.to_owned()));
        }
        let end: Id = self.to_id(tokens[1])?;
        Ok(if start == UNASSIGNED {
            crate::Move::Place(end, bug)
        } else {
            crate::Move::Movement(start, end)
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
                self.id_to_name_stack.get(&start).unwrap().last().unwrap().clone()
            }
            crate::Move::Pass => return "pass".to_owned(),
        };
        if self.move_history.len() == 0 {
            return move_string;
        }
        move_string.push(' ');

        let end: Id = match m {
            crate::Move::Place(id, _) => id,
            crate::Move::Movement(_, end) => end,
            crate::Move::Pass => unreachable!(),
        };
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

    pub(crate) fn apply(&mut self, m: crate::Move) -> Result<()> {
        match m {
            crate::Move::Place(id, bug) => {
                let color = self.board.to_move();
                let name = bug_name(color, bug, self.next_bug_num(bug));
                self.name_to_id.insert(name.clone(), id);
                self.id_to_name_stack.insert(id, vec![name]);
            }
            crate::Move::Movement(start, end) => {
                let name = self.id_to_name_stack.get_mut(&start).unwrap().pop().unwrap();
                self.name_to_id.insert(name.clone(), end);
                self.id_to_name_stack.entry(end).or_insert_with(|| Vec::new()).push(name);
            }
            crate::Move::Pass => {}
        }
        m.apply(&mut self.board);
        self.move_history.push(m);
        Ok(())
    }

    pub(crate) fn undo(&mut self) {
        let m = self.move_history.pop().unwrap();
        match m {
            crate::Move::Place(id, _) => {
                let name = self.id_to_name_stack.get_mut(&id).unwrap().pop().unwrap();
                self.name_to_id.insert(name, UNASSIGNED);
            }
            crate::Move::Movement(start, end) => {
                let name = self.id_to_name_stack.get_mut(&end).unwrap().pop().unwrap();
                self.name_to_id.insert(name.clone(), start);
                self.id_to_name_stack.entry(start).or_insert_with(|| Vec::new()).push(name);
            }
            crate::Move::Pass => {}
        }
        m.undo(&mut self.board);
    }

    pub(crate) fn game_log(&mut self) -> String {
        let history = self.move_history.clone();
        for _ in 0..history.len() {
            self.undo();
        }
        let mut log = String::new();
        for &m in history.iter() {
            log.push_str(&self.to_move_string(m));
            log.push(';');
            self.apply(m).unwrap();
        }
        if log.ends_with(";") {
            log.pop();
        }
        log
    }
}

#[cfg(test)]
mod tests {
    extern crate rand;
    use super::*;
    use minimax::Game;
    use rand::Rng;

    #[test]
    fn test_move_string_round_trip() {
        let mut b = UhpBoard::new("Base+MLP");
        let mut rng = rand::thread_rng();
        for iter in 0..20 {
            let mut moves = [None; 200];
            let mut depth = 0;
            for _ in 0..20 {
                depth += 1;
                let n = crate::Game::generate_moves(&b.board, &mut moves);
                let m = moves[rng.gen_range(0, n)].unwrap();
                let move_string = b.to_move_string(m);
                assert_eq!(
                    m,
                    b.from_move_string(&move_string).unwrap(),
                    "iter={}, move_string={}\n{}",
                    iter,
                    move_string,
                    b.board
                );
                b.apply(m).unwrap();
                if crate::Game::get_winner(&b.board).is_some() {
                    break;
                }
            }
            for _ in 0..depth {
                b.undo();
            }
        }
    }
}
