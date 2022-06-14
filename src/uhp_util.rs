extern crate minimax;
use crate::{adjacent, Board, Bug, Color, Id, Node, Rules, GRID_MASK, ROW_SIZE, START_ID};
use minimax::{Game, Move};

#[derive(Debug)]
pub enum UhpError {
    IoError(std::io::Error),
    UnknownPiece(String),
    InvalidGameString(String),
    InvalidGameType(String),
    InvalidMove(String),
    GameNotStarted,
    UnrecognizedCommand(String),
    EngineError(String),
    TooManyUndos,
}

impl From<std::io::Error> for UhpError {
    fn from(error: std::io::Error) -> Self {
        UhpError::IoError(error)
    }
}

pub type Result<T> = std::result::Result<T, UhpError>;

impl Board {
    // New board from UHP GameTypeString, e.g. "Base+MLP"
    pub fn from_game_type(game_type: &str) -> Result<Self> {
        let err = || UhpError::InvalidGameType(game_type.to_owned());
        let mut starting = [1, 3, 2, 3, 2, 0, 0, 0];
        let mut toks = game_type.split('+');
        if toks.next().ok_or_else(err)? != "Base" {
            return Err(err());
        }
        if let Some(exts) = toks.next() {
            for ext in exts.chars() {
                match ext {
                    'M' => starting[Bug::Mosquito as usize] = 1,
                    'L' => starting[Bug::Ladybug as usize] = 1,
                    'P' => starting[Bug::Pillbug as usize] = 1,
                    _ => return Err(err()),
                }
            }
        }
        Ok(Board::new(starting))
    }

    pub(super) fn id_name(&self, id: Id, out: &mut String) {
        if self.occupied(id) {
            self.tile_name(self.node(id), out);
            return;
        }
        // Name this relative to an adjacent tile.
        for (dir, adj) in (0..6).zip(adjacent(id).into_iter()) {
            if self.occupied(adj) {
                // Reverse directions; they're from the other bug's perspective.
                out.push_str(match dir {
                    3 => "\\",
                    2 => "-",
                    1 => "/",
                    _ => "",
                });
                self.tile_name(self.node(adj), out);
                out.push_str(match dir {
                    4 => "/",
                    5 => "-",
                    0 => "\\",
                    _ => "",
                });
                return;
            }
        }
        out.push_str("??");
    }

    pub(super) fn tile_name(&self, node: Node, out: &mut String) {
        out.push(match node.color() {
            Color::White => 'w',
            Color::Black => 'b',
        });
        out.push(node.bug().to_char().to_ascii_uppercase());
        if matches!(node.bug(), Bug::Ant | Bug::Grasshopper | Bug::Beetle | Bug::Spider) {
            out.push(char::from_digit(node.bug_num() as u32, 10).unwrap());
        }
    }

    pub(super) fn new_tile_name(&self, bug: Bug, out: &mut String) {
        out.push(match self.to_move() {
            Color::White => 'w',
            Color::Black => 'b',
        });
        out.push(bug.to_char().to_ascii_uppercase());
        if matches!(bug, Bug::Ant | Bug::Grasshopper | Bug::Beetle | Bug::Spider) {
            let bug_num =
                Bug::initial_quantity()[bug as usize] - self.get_remaining()[bug as usize] + 1;
            out.push(char::from_digit(bug_num as u32, 10).unwrap());
        }
    }

    pub(super) fn to_move_string(&self, m: crate::Move) -> String {
        let mut out = String::new();
        match m {
            crate::Move::Movement(start, _) => {
                if !self.occupied(start) {
                    return "??".to_string();
                }
                self.tile_name(self.node(start), &mut out);
            }
            crate::Move::Place(_, bug) => self.new_tile_name(bug, &mut out),
            crate::Move::Pass => return "pass".to_string(),
        }

        if self.move_num == 0 {
            return out;
        }
        out.push(' ');

        match m {
            crate::Move::Movement(_, end) => self.id_name(end, &mut out),
            crate::Move::Place(id, _) => self.id_name(id, &mut out),
            crate::Move::Pass => unreachable!(),
        }
        out
    }

    pub fn game_string(&self) -> String {
        let mut out = self.game_type();
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

    pub fn game_type(&self) -> String {
        let mut game_type = "Base".to_string();
        let mosquito = self.game_type_bits & (1 << Bug::Mosquito as u32) != 0;
        let ladybug = self.game_type_bits & (1 << Bug::Ladybug as u32) != 0;
        let pillbug = self.game_type_bits & (1 << Bug::Pillbug as u32) != 0;
        if mosquito || ladybug || pillbug {
            game_type.push('+');
        }
        if mosquito {
            game_type.push('M');
        }
        if ladybug {
            game_type.push('L');
        }
        if pillbug {
            game_type.push('P');
        }
        game_type
    }

    pub fn game_state_string(&self) -> &'static str {
        if self.move_history.is_empty() {
            return "NotStarted";
        }
        match Rules::get_winner(&self) {
            Some(minimax::Winner::Draw) => "Draw",
            Some(minimax::Winner::PlayerToMove) => match self.to_move() {
                Color::Black => "BlackWins",
                Color::White => "WhiteWins",
            },
            Some(minimax::Winner::PlayerJustMoved) => match self.to_move() {
                Color::White => "BlackWins",
                Color::Black => "WhiteWins",
            },
            None => "InProgress",
        }
    }

    fn turn_string(&self) -> String {
        format!("{:?}[{}]", self.to_move(), self.move_history.len() / 2 + 1)
    }

    pub fn game_log(&self) -> String {
        let mut board = Board::from_game_type(&self.game_type()).unwrap();
        let mut log = String::new();
        for &m in &self.move_history {
            log.push_str(&board.to_move_string(m));
            log.push(';');
            m.apply(&mut board);
        }
        if log.ends_with(';') {
            log.pop();
        }
        log
    }

    // From e.g. "wB2-", returns bug, num, color, delta (White, Beetle, 2, 1)
    fn parse_piece_name(&self, mut piece_string: &str) -> Option<(Color, Bug, u8, Id)> {
        let first = piece_string.chars().next()?;
        let last = piece_string.chars().rev().next()?;
        let delta = if "\\-/".contains(first) {
            piece_string = &piece_string[1..];
            match first {
                '\\' => (ROW_SIZE + 1).wrapping_neg(),
                '-' => (1 as Id).wrapping_neg(),
                '/' => ROW_SIZE,
                _ => return None,
            }
        } else if "\\-/".contains(last) {
            piece_string = &piece_string[..piece_string.len() - 1];
            match last {
                '/' => ROW_SIZE.wrapping_neg(),
                '-' => 1,
                '\\' => ROW_SIZE + 1,
                _ => return None,
            }
        } else {
            0
        };

        let mut chars = piece_string.chars();
        let color = match chars.next()? {
            'w' => Color::White,
            'b' => Color::Black,
            _ => return None,
        };
        let bug = Bug::from_char(chars.next()?)?;
        let bug_num = if Bug::initial_quantity()[bug as usize] > 1 {
            char::to_digit(chars.next()?, 10)? as u8
        } else if chars.next().is_some() {
            return None;
        } else {
            1
        };
        Some((color, bug, bug_num, delta))
    }

    // Returns the location of the piece, or None if it is not on the board.
    pub(crate) fn find_bug(&self, color: Color, bug: Bug, bug_num: u8) -> Option<Id> {
        self.occupied_ids[color as usize]
            .iter()
            .copied()
            .find(|&id| {
                let node = self.node(id);
                node.bug() == bug && node.bug_num() == bug_num
            })
            .or_else(|| {
                self.get_underworld()
                    .iter()
                    .find(|under| {
                        under.node().color() == color
                            && under.node().bug() == bug
                            && under.node().bug_num() == bug_num
                    })
                    .map(|under| under.id())
            })
    }

    // https://github.com/jonthysell/Mzinga/wiki/UniversalHiveProtocol#movestring
    pub(crate) fn from_move_string(&self, move_string: &str) -> Result<crate::Move> {
        let err = || UhpError::InvalidMove(move_string.to_owned());
        if move_string == "pass" {
            return Ok(crate::Move::Pass);
        }
        let tokens = move_string.split(' ').collect::<Vec<_>>();
        let (color, bug, bug_num, delta) = self.parse_piece_name(tokens[0]).ok_or_else(err)?;
        if delta != 0 {
            return Err(err());
        }
        if self.move_history.is_empty() {
            if tokens.len() != 1 {
                return Err(err());
            }
            return Ok(crate::Move::Place(START_ID, bug));
        }
        if tokens.len() != 2 {
            return Err(err());
        }
        let start: Option<Id> = self.find_bug(color, bug, bug_num);
        let end: Id = {
            let (color, bug, bug_num, delta) = self.parse_piece_name(tokens[1]).ok_or_else(err)?;
            let id = self.find_bug(color, bug, bug_num).ok_or_else(err)?;
            id.wrapping_add(delta) & GRID_MASK
        };
        Ok(if start.is_some() && self.occupied(start.unwrap()) {
            crate::Move::Movement(start.unwrap(), end)
        } else {
            if color != self.to_move() {
                return Err(err());
            }
            let expected_bug_num =
                Bug::initial_quantity()[bug as usize] - self.get_remaining()[bug as usize] + 1;
            if bug_num != expected_bug_num {
                return Err(err());
            }
            crate::Move::Place(end, bug)
        })
    }

    pub(crate) fn from_game_string(s: &str) -> Result<Self> {
        let mut toks = s.split(';');
        let game_type = toks.next().ok_or_else(|| UhpError::InvalidGameString(s.to_owned()))?;
        let mut board = Board::from_game_type(game_type)?;
        // We don't actually care about the game state, but
        // we'll just treat this like a game type if it's
        if toks.next().is_none() {
            return Ok(board);
        }
        // Don't care about turn string either.
        // Although it would say which color moved first?
        toks.next().ok_or_else(|| UhpError::InvalidGameString(s.to_owned()))?;
        // The rest are move strings.
        for move_string in toks {
            let m = board.from_move_string(move_string)?;
            board.apply_untrusted(m)?;
        }
        Ok(board)
    }

    pub(crate) fn apply_untrusted(&mut self, m: crate::Move) -> Result<()> {
        let mut moves = Vec::new();
        Rules::generate_moves(&self, &mut moves);
        if !moves.contains(&m) {
            return Err(UhpError::InvalidMove("That is not a valid move".to_string()));
        }
        m.apply(self);
        Ok(())
    }

    pub(crate) fn undo_count(&mut self, count: usize) -> Result<()> {
        for _ in 0..count {
            let m = self.move_history.last().copied().ok_or(UhpError::TooManyUndos)?;
            m.undo(self);
        }
        Ok(())
    }

    pub(crate) fn last_move(&self) -> Option<crate::Move> {
        self.move_history.last().copied()
    }

    pub(crate) fn valid_moves(&self) -> String {
        let mut moves = Vec::new();
        Rules::generate_moves(&self, &mut moves);
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Rules;
    use minimax::{Game, Strategy};

    #[test]
    fn test_move_string_round_trip() {
        let mut board = Board::from_game_type("Base+MLP").unwrap();
        let mut rand = minimax::Random::<Rules>::new();
        for iter in 0..20 {
            let mut depth = 0;
            for _ in 0..20 {
                depth += 1;
                let m = rand.choose_move(&board).unwrap();
                let move_string = board.to_move_string(m);
                assert_eq!(
                    m,
                    board.from_move_string(&move_string).unwrap(),
                    "iter={}, move_string={}",
                    iter,
                    move_string,
                );
                board.apply_untrusted(m).unwrap();
                if Rules::get_winner(&board).is_some() {
                    break;
                }
            }
            board.undo_count(depth).unwrap();
        }
    }
}
