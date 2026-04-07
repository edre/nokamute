//! Python bindings for nokamute via PyO3.
//!
//! Build with: `maturin develop --features python`

use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;

use crate::board::{Board, Color, Turn, hex_to_loc};
use crate::bug::Bug;
use crate::eval::BasicEvaluator;

use minimax::{Game, Evaluator};

/// A Hive game move.
#[pyclass]
#[derive(Clone)]
pub struct PyMove {
    pub(crate) turn: Turn,
    #[pyo3(get)]
    pub move_type: String,
    #[pyo3(get)]
    pub piece_type: Option<String>,
    #[pyo3(get)]
    pub source_q: Option<i8>,
    #[pyo3(get)]
    pub source_r: Option<i8>,
    #[pyo3(get)]
    pub dest_q: Option<i8>,
    #[pyo3(get)]
    pub dest_r: Option<i8>,
}

#[pymethods]
impl PyMove {
    fn __repr__(&self) -> String {
        match self.move_type.as_str() {
            "place" => format!(
                "Move(place, {}, ({}, {}))",
                self.piece_type.as_deref().unwrap_or("?"),
                self.dest_q.unwrap_or(0),
                self.dest_r.unwrap_or(0),
            ),
            "move" => format!(
                "Move(move, ({}, {})->({}, {}))",
                self.source_q.unwrap_or(0),
                self.source_r.unwrap_or(0),
                self.dest_q.unwrap_or(0),
                self.dest_r.unwrap_or(0),
            ),
            _ => "Move(pass)".to_string(),
        }
    }

    /// Canonical sort key for deterministic move ordering.
    /// Uses a custom piece ordering (not Bug enum discriminant order)
    /// to group related pieces for more intuitive action-space layout.
    fn sort_key(&self) -> (u8, u8, i8, i8, i8, i8) {
        let move_type_ord = match self.move_type.as_str() {
            "place" => 0,
            "move" => 1,
            _ => 2,
        };
        let piece_ord = self.piece_type.as_ref().map_or(255, |p| bug_name_to_sort_ord(p));
        (
            move_type_ord,
            piece_ord,
            self.source_q.unwrap_or(0),
            self.source_r.unwrap_or(0),
            self.dest_q.unwrap_or(0),
            self.dest_r.unwrap_or(0),
        )
    }
}

#[pyclass]
#[derive(Clone)]
pub struct PyPiece {
    #[pyo3(get)]
    pub piece_type: String,
    #[pyo3(get)]
    pub color: i8,
    #[pyo3(get)]
    pub q: i8,
    #[pyo3(get)]
    pub r: i8,
    #[pyo3(get)]
    pub stack_height: u8,
}

#[pymethods]
impl PyPiece {
    fn __repr__(&self) -> String {
        format!(
            "Piece({}, color={}, ({}, {}), height={})",
            self.piece_type, self.color, self.q, self.r, self.stack_height
        )
    }

    fn as_tuple(&self) -> (String, i8, i8, i8, u8) {
        (self.piece_type.clone(), self.color, self.q, self.r, self.stack_height)
    }
}

#[pyclass]
pub struct GameState {
    board: Board,
}

fn color_to_player(c: Color) -> i8 {
    match c {
        Color::White => 1,
        Color::Black => -1,
    }
}

#[pymethods]
impl GameState {
    /// Optionally pass a UHP game string to restore state.
    #[new]
    #[pyo3(signature = (uhp_string=None))]
    fn new(uhp_string: Option<&str>) -> PyResult<Self> {
        match uhp_string {
            Some(s) => {
                let board = Board::from_game_string(s)
                    .map_err(|e| PyValueError::new_err(format!("Invalid UHP string: {:?}", e)))?;
                Ok(GameState { board })
            }
            None => Ok(GameState {
                board: Board::new_expansions(),
            }),
        }
    }

    fn clone(&self) -> GameState {
        GameState {
            board: self.board.clone(),
        }
    }

    /// 1 for White, -1 for Black.
    fn current_player(&self) -> i8 {
        color_to_player(self.board.to_move())
    }

    fn turn_number(&self) -> u16 {
        self.board.turn_num
    }

    fn is_over(&self) -> bool {
        crate::board::Rules::get_winner(&self.board).is_some()
    }

    /// Returns 1 (White), -1 (Black), 0 (draw), or None if game is not over.
    fn game_result(&self) -> Option<i8> {
        match crate::board::Rules::get_winner(&self.board) {
            Some(minimax::Winner::PlayerToMove) => Some(self.current_player()),
            Some(minimax::Winner::PlayerJustMoved) => Some(-self.current_player()),
            Some(minimax::Winner::Draw) => Some(0),
            None => None,
        }
    }

    /// Returns 1 (White), -1 (Black), 0 (draw or not over).
    fn winner(&self) -> i8 {
        self.game_result().unwrap_or(0)
    }

    fn legal_moves(&self) -> Vec<PyMove> {
        let mut turns = Vec::new();
        crate::board::Rules::generate_moves(&self.board, &mut turns);
        let mut moves = Vec::with_capacity(turns.len());
        for t in turns {
            moves.push(turn_to_pymove(&self.board, t));
        }
        moves
    }

    fn apply_move(&mut self, pymove: &PyMove) {
        self.board.apply(pymove.turn);
    }

    fn undo_move(&mut self, pymove: &PyMove) {
        self.board.undo(pymove.turn);
    }

    fn pieces(&self) -> Vec<PyPiece> {
        let capacity = self.board.occupied_hexes[0].len()
            + self.board.occupied_hexes[1].len()
            + self.board.get_underworld().len();
        let mut result = Vec::with_capacity(capacity);

        for color_idx in 0..2 {
            for &hex in &self.board.occupied_hexes[color_idx] {
                let node = self.board.node(hex);
                let (q, r) = hex_to_loc(hex);
                result.push(PyPiece {
                    piece_type: node.bug().name().to_string(),
                    color: color_to_player(node.color()),
                    q,
                    r,
                    stack_height: node.clipped_height().max(1),
                });
            }
        }

        for under in self.board.get_underworld() {
            let node = under.node();
            if !node.occupied() {
                continue;
            }
            let (q, r) = hex_to_loc(under.hex());
            result.push(PyPiece {
                piece_type: node.bug().name().to_string(),
                color: color_to_player(node.color()),
                q,
                r,
                stack_height: node.clipped_height().max(1),
            });
        }

        result
    }

    fn game_string(&self) -> String {
        self.board.game_string()
    }

    fn move_string(&self, pymove: &PyMove) -> String {
        self.board.to_move_string(pymove.turn)
    }

    fn parse_move(&self, move_string: &str) -> PyResult<PyMove> {
        let turn = self.board.from_move_string(move_string)
            .map_err(|e| PyValueError::new_err(format!("Invalid move string: {:?}", e)))?;
        Ok(turn_to_pymove(&self.board, turn))
    }

    fn valid_moves_uhp(&self) -> String {
        self.board.valid_moves()
    }

    /// Heuristic score from the current player's perspective.
    fn evaluate(&self) -> f64 {
        let eval = BasicEvaluator::default();
        eval.evaluate(&self.board) as f64
    }

    fn game_type(&self) -> String {
        self.board.game_type()
    }

    fn queen_placed(&self, player: i8) -> PyResult<bool> {
        let color_idx = match player {
            1 => 0,
            -1 => 1,
            _ => return Err(PyValueError::new_err("player must be 1 or -1")),
        };
        Ok(self.board.remaining[color_idx][Bug::Queen as usize] == 0)
    }

    /// Kept for convenience; prefer queen_placed(1) / queen_placed(-1).
    fn white_queen_placed(&self) -> bool {
        self.board.remaining[0][Bug::Queen as usize] == 0
    }

    fn black_queen_placed(&self) -> bool {
        self.board.remaining[1][Bug::Queen as usize] == 0
    }

    /// Returns (white_neighbor_count, black_neighbor_count).
    fn queens_neighbor_count(&self) -> (usize, usize) {
        let qs = self.board.queens_surrounded();
        (qs[0], qs[1])
    }
}

// --- Helpers ---

/// Sort ordinal for piece types in the canonical action-space ordering.
/// Intentionally differs from Bug enum discriminants to group pieces intuitively.
fn bug_name_to_sort_ord(name: &str) -> u8 {
    match name {
        "queen" => 0,
        "beetle" => 1,
        "grasshopper" => 2,
        "ant" => 3,
        "spider" => 4,
        "mosquito" => 5,
        "ladybug" => 6,
        "pillbug" => 7,
        _ => 255,
    }
}

fn turn_to_pymove(board: &Board, turn: Turn) -> PyMove {
    match turn {
        Turn::Place(hex, bug) => {
            let (q, r) = hex_to_loc(hex);
            PyMove {
                turn,
                move_type: "place".to_string(),
                piece_type: Some(bug.name().to_string()),
                source_q: None,
                source_r: None,
                dest_q: Some(q),
                dest_r: Some(r),
            }
        }
        Turn::Move(src, dst) => {
            let (sq, sr) = hex_to_loc(src);
            let (dq, dr) = hex_to_loc(dst);
            let node = board.node(src);
            PyMove {
                turn,
                move_type: "move".to_string(),
                piece_type: Some(node.bug().name().to_string()),
                source_q: Some(sq),
                source_r: Some(sr),
                dest_q: Some(dq),
                dest_r: Some(dr),
            }
        }
        Turn::Pass => PyMove {
            turn,
            move_type: "pass".to_string(),
            piece_type: None,
            source_q: None,
            source_r: None,
            dest_q: None,
            dest_r: None,
        },
    }
}

#[pymodule]
pub fn nokamute(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<GameState>()?;
    m.add_class::<PyMove>()?;
    m.add_class::<PyPiece>()?;
    Ok(())
}
