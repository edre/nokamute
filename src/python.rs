//! Python bindings for nokamute via PyO3.
//!
//! Exposes GameState, Move, and related types for use from Python.
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

    /// Return a canonical sort key for deterministic ordering.
    fn sort_key(&self) -> (u8, u8, i8, i8, i8, i8) {
        let move_type_ord = match self.move_type.as_str() {
            "place" => 0,
            "move" => 1,
            _ => 2,
        };
        let piece_ord = self.piece_type.as_ref().map_or(255, |p| bug_name_to_index(p));
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

/// Piece information tuple: (piece_type, color, q, r, stack_height)
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

/// Main game state wrapper around nokamute's Board.
#[pyclass]
pub struct GameState {
    board: Board,
}

#[pymethods]
impl GameState {
    /// Create a new game. Optionally pass a UHP game string to restore state.
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

    /// Deep clone the game state.
    fn clone(&self) -> GameState {
        GameState {
            board: self.board.clone(),
        }
    }

    /// Get the current player: 1 for White, -1 for Black.
    fn current_player(&self) -> i8 {
        match self.board.to_move() {
            Color::White => 1,
            Color::Black => -1,
        }
    }

    /// Get the current turn number (0-indexed).
    fn turn_number(&self) -> u16 {
        self.board.turn_num
    }

    /// Check if the game is over.
    fn is_over(&self) -> bool {
        crate::board::Rules::get_winner(&self.board).is_some()
    }

    /// Get the winner: 1 (White), -1 (Black), 0 (draw).
    /// Returns 0 if game is not over.
    fn winner(&self) -> i8 {
        match crate::board::Rules::get_winner(&self.board) {
            Some(minimax::Winner::PlayerToMove) => self.current_player(),
            Some(minimax::Winner::PlayerJustMoved) => -self.current_player(),
            Some(minimax::Winner::Draw) => 0,
            None => 0,
        }
    }

    /// Get all legal moves as a list of PyMove objects.
    fn legal_moves(&self) -> Vec<PyMove> {
        let mut turns = Vec::new();
        crate::board::Rules::generate_moves(&self.board, &mut turns);
        turns.into_iter().map(|t| turn_to_pymove(&self.board, t)).collect()
    }

    /// Apply a move to the game state (mutates in place).
    fn apply_move(&mut self, pymove: &PyMove) {
        self.board.apply(pymove.turn);
    }

    /// Undo the last applied move.
    fn undo_move(&mut self, pymove: &PyMove) {
        self.board.undo(pymove.turn);
    }

    /// Get all pieces on the board, including stacked pieces.
    /// Returns a list of PyPiece objects with (piece_type, color, q, r, stack_height).
    fn pieces(&self) -> Vec<PyPiece> {
        let mut result = Vec::new();

        for color_idx in 0..2 {

            for &hex in &self.board.occupied_hexes[color_idx] {
                let node = self.board.node(hex);
                let (q, r) = hex_to_loc(hex);
                let height = node.clipped_height();

                // Top piece
                result.push(PyPiece {
                    piece_type: bug_to_string(node.bug()),
                    color: if node.color() == Color::White { 1 } else { -1 },
                    q,
                    r,
                    stack_height: height.max(1),
                });
            }
        }

        // Add underworld pieces (pieces under stacks)
        for under in self.board.get_underworld() {
            let node = under.node();
            if !node.occupied() {
                continue;
            }
            let (q, r) = hex_to_loc(under.hex());
            result.push(PyPiece {
                piece_type: bug_to_string(node.bug()),
                color: if node.color() == Color::White { 1 } else { -1 },
                q,
                r,
                stack_height: node.clipped_height().max(1),
            });
        }

        result
    }

    /// Get the UHP game string representation.
    fn game_string(&self) -> String {
        self.board.game_string()
    }

    /// Get the UHP move string for a move.
    fn move_string(&self, pymove: &PyMove) -> String {
        self.board.to_move_string(pymove.turn)
    }

    /// Parse a UHP move string into a PyMove.
    fn parse_move(&self, move_string: &str) -> PyResult<PyMove> {
        let turn = self.board.from_move_string(move_string)
            .map_err(|e| PyValueError::new_err(format!("Invalid move string: {:?}", e)))?;
        Ok(turn_to_pymove(&self.board, turn))
    }

    /// Get all valid moves as UHP strings.
    fn valid_moves_uhp(&self) -> String {
        self.board.valid_moves()
    }

    /// Evaluate the current position using nokamute's heuristic.
    /// Returns a score from the current player's perspective.
    fn evaluate(&self) -> f64 {
        let eval = BasicEvaluator::default();
        let score = eval.evaluate(&self.board);
        score as f64
    }

    /// Get the game type string (e.g., "Base+MLP").
    fn game_type(&self) -> String {
        self.board.game_type()
    }

    /// Check if White's queen has been placed.
    fn white_queen_placed(&self) -> bool {
        self.board.remaining[0][Bug::Queen as usize] == 0
    }

    /// Check if Black's queen has been placed.
    fn black_queen_placed(&self) -> bool {
        self.board.remaining[1][Bug::Queen as usize] == 0
    }

    /// Get the number of neighbors surrounding each queen: [black_count, white_count].
    fn queens_neighbor_count(&self) -> (usize, usize) {
        let qs = self.board.queens_surrounded();
        (qs[0], qs[1])
    }
}

// Helper functions

fn bug_to_string(bug: Bug) -> String {
    match bug {
        Bug::Queen => "queen",
        Bug::Grasshopper => "grasshopper",
        Bug::Spider => "spider",
        Bug::Ant => "ant",
        Bug::Beetle => "beetle",
        Bug::Mosquito => "mosquito",
        Bug::Ladybug => "ladybug",
        Bug::Pillbug => "pillbug",
    }.to_string()
}

fn bug_name_to_index(name: &str) -> u8 {
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
                piece_type: Some(bug_to_string(bug)),
                source_q: None,
                source_r: None,
                dest_q: Some(q),
                dest_r: Some(r),
            }
        }
        Turn::Move(src, dst) => {
            let (sq, sr) = hex_to_loc(src);
            let (dq, dr) = hex_to_loc(dst);
            // Get the piece type at the source
            let node = board.node(src);
            let piece_type = if node.occupied() {
                Some(bug_to_string(node.bug()))
            } else {
                None
            };
            PyMove {
                turn,
                move_type: "move".to_string(),
                piece_type,
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

/// nokamute Python module
#[pymodule]
pub fn nokamute(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<GameState>()?;
    m.add_class::<PyMove>()?;
    m.add_class::<PyPiece>()?;
    Ok(())
}
