//! Python bindings for nokamute via PyO3.
//!
//! Build with: `maturin develop --features python`

#[cfg(feature = "smaller-grid")]
compile_error!("Python bindings require the default 32x32 grid. Disable the `smaller-grid` feature.");

use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;

use crate::board::{Board, Color, Turn, hex_to_loc};
use crate::bug::Bug;
use crate::eval::BasicEvaluator;
use crate::hex_grid::{Hex, GRID_SIZE};

use minimax::{Game, Evaluator, IterativeOptions, IterativeSearch, Strategy};
use numpy::ndarray::Array3;
use std::time::Duration;
use numpy::{IntoPyArray, PyArray3};

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
#[derive(Clone)]
pub struct PySearchResult {
    #[pyo3(get)]
    pub best_move: PyMove,
    #[pyo3(get)]
    pub score: f64,
    #[pyo3(get)]
    pub depth: u8,
}

#[pymethods]
impl PySearchResult {
    fn __repr__(&self) -> String {
        format!(
            "SearchResult(move={}, score={:.1}, depth={})",
            self.best_move.__repr__(),
            self.score,
            self.depth,
        )
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

    /// Search for the best move using iterative deepening alpha-beta.
    /// At least one of `depth` or `timeout_ms` must be provided.
    #[pyo3(signature = (depth=None, timeout_ms=None))]
    fn search(&self, depth: Option<u8>, timeout_ms: Option<u64>) -> PyResult<PyMove> {
        let result = self.run_search(depth, timeout_ms)?;
        Ok(result.best_move)
    }

    /// Like search(), but returns a PySearchResult with score and depth info.
    #[pyo3(signature = (depth=None, timeout_ms=None))]
    fn search_with_info(&self, depth: Option<u8>, timeout_ms: Option<u64>) -> PyResult<PySearchResult> {
        self.run_search(depth, timeout_ms)
    }

    /// Returns the board tensor as a numpy array of shape (84, 32, 32).
    fn encode_board<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyArray3<f32>>> {
        let data = self.encode_board_data();
        let array = Array3::from_shape_vec((84, 32, 32), data)
            .map_err(|e| PyValueError::new_err(format!("Shape error: {}", e)))?;
        Ok(array.into_pyarray(py))
    }
}

// --- Helpers ---

/// Canonical sort ordinal for piece types in the action-space and tensor encoding.
/// Intentionally differs from Bug enum discriminants to group pieces intuitively.
fn bug_sort_ord(bug: Bug) -> u8 {
    match bug {
        Bug::Queen => 0,
        Bug::Beetle => 1,
        Bug::Grasshopper => 2,
        Bug::Ant => 3,
        Bug::Spider => 4,
        Bug::Mosquito => 5,
        Bug::Ladybug => 6,
        Bug::Pillbug => 7,
    }
}

fn bug_name_to_sort_ord(name: &str) -> u8 {
    name.chars()
        .next()
        .and_then(Bug::from_char)
        .map(bug_sort_ord)
        .unwrap_or(255)
}

/// Pure-Rust search result (no PyO3 dependency) for testability.
struct RawSearchResult {
    turn: Turn,
    score: f64,
    depth: u8,
}

/// Run iterative-deepening alpha-beta search. Pure Rust, no PyO3 types.
fn run_search_raw(
    board: &Board, depth: Option<u8>, timeout_ms: Option<u64>,
) -> Result<RawSearchResult, &'static str> {
    if depth.is_none() && timeout_ms.is_none() {
        return Err("At least one of `depth` or `timeout_ms` must be provided");
    }

    let opts = IterativeOptions::new()
        .with_table_byte_size(16 << 20)
        .with_countermoves()
        .with_countermove_history();
    let mut strategy = IterativeSearch::new(BasicEvaluator::default(), opts);

    match (depth, timeout_ms) {
        (Some(d), Some(t)) => {
            strategy.set_depth_or_timeout(d, Duration::from_millis(t));
        }
        (Some(d), None) => {
            strategy.set_max_depth(d);
        }
        (None, Some(t)) => {
            strategy.set_timeout(Duration::from_millis(t));
        }
        (None, None) => unreachable!(),
    }

    let turn = strategy.choose_move(board).ok_or("No legal moves available")?;
    let score = strategy.root_value() as f64;
    let pv_depth = strategy.principal_variation().len() as u8;

    Ok(RawSearchResult { turn, score, depth: pv_depth })
}

impl GameState {
    fn run_search(&self, depth: Option<u8>, timeout_ms: Option<u64>) -> PyResult<PySearchResult> {
        let raw = run_search_raw(&self.board, depth, timeout_ms)
            .map_err(PyValueError::new_err)?;
        Ok(PySearchResult {
            best_move: turn_to_pymove(&self.board, raw.turn),
            score: raw.score,
            depth: raw.depth,
        })
    }

    fn encode_board_data(&self) -> Vec<f32> {
        const BOARD_SIZE: usize = 32;
        const CENTER: i32 = 16;
        const PLANE_SIZE: usize = BOARD_SIZE * BOARD_SIZE;
        const NUM_CHANNELS: usize = 84;
        const MAX_STACK: usize = 5;

        let mut data = vec![0.0f32; NUM_CHANNELS * PLANE_SIZE];
        let (offset_q, offset_r) = self.encode_offset();

        let mut stack_counts = [0u8; GRID_SIZE];
        let mut stack_data = [[(0u8, Bug::Queen, Color::White); MAX_STACK]; GRID_SIZE];

        for color_idx in 0..2 {
            for &hex in &self.board.occupied_hexes[color_idx] {
                let node = self.board.node(hex);
                let idx = hex as usize;
                let n = stack_counts[idx] as usize;
                if n < MAX_STACK {
                    stack_data[idx][n] =
                        (node.clipped_height().max(1), node.bug(), node.color());
                    stack_counts[idx] += 1;
                }
            }
        }
        for under in self.board.get_underworld() {
            let node = under.node();
            if !node.occupied() {
                continue;
            }
            let idx = under.hex() as usize;
            let n = stack_counts[idx] as usize;
            if n < MAX_STACK {
                stack_data[idx][n] = (under.height(), node.bug(), node.color());
                stack_counts[idx] += 1;
            }
        }

        for hex_idx in 0..GRID_SIZE {
            let n = stack_counts[hex_idx] as usize;
            if n == 0 {
                continue;
            }
            let (q, r) = hex_to_loc(hex_idx as Hex);
            let bq = q as i32 - offset_q + CENTER;
            let br = r as i32 - offset_r + CENTER;
            if bq < 0 || bq >= BOARD_SIZE as i32 || br < 0 || br >= BOARD_SIZE as i32 {
                continue;
            }
            let spatial_idx = bq as usize * BOARD_SIZE + br as usize;

            let pieces = &mut stack_data[hex_idx][..n];
            pieces.sort_by_key(|&(h, _, _)| h);

            for (layer_idx, &(_, bug, color)) in pieces.iter().enumerate() {
                let color_offset = if color == Color::White { 0 } else { 8 };
                let channel = layer_idx * 16 + color_offset + bug_sort_ord(bug) as usize;
                data[channel * PLANE_SIZE + spatial_idx] = 1.0;
            }
        }

        let current_player = if self.board.to_move() == Color::White { 1.0f32 } else { 0.0 };
        // 200 is a generous upper bound on Hive game length for normalization.
        let turn_normalized = self.board.turn_num as f32 / 200.0;
        let white_qp = if self.white_queen_placed() { 1.0f32 } else { 0.0 };
        let black_qp = if self.black_queen_placed() { 1.0f32 } else { 0.0 };

        for (i, &value) in [current_player, turn_normalized, white_qp, black_qp].iter().enumerate()
        {
            let offset = (80 + i) * PLANE_SIZE;
            data[offset..offset + PLANE_SIZE].fill(value);
        }

        data
    }

    fn encode_offset(&self) -> (i32, i32) {
        if self.white_queen_placed() {
            let (q, r) = hex_to_loc(self.board.queens[0]);
            return (q as i32, r as i32);
        }
        let mut sum_q = 0i32;
        let mut sum_r = 0i32;
        let mut count = 0i32;
        for color_hexes in &self.board.occupied_hexes {
            for &hex in color_hexes {
                let (q, r) = hex_to_loc(hex);
                sum_q += q as i32;
                sum_r += r as i32;
                count += 1;
            }
        }
        if count > 0 {
            let avg_q = (sum_q as f32 / count as f32).round() as i32;
            let avg_r = (sum_r as f32 / count as f32).round() as i32;
            (avg_q, avg_r)
        } else {
            (0, 0)
        }
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
    m.add_class::<PySearchResult>()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::board::{loc_to_hex, Turn};

    const P: usize = 32 * 32; // plane size

    fn make_state() -> GameState {
        GameState { board: Board::new_expansions() }
    }

    fn assert_legal_move(board: &Board, turn: Turn) {
        let mut moves = Vec::new();
        crate::board::Rules::generate_moves(board, &mut moves);
        assert!(moves.contains(&turn), "move {:?} is not legal", turn);
    }

    #[test]
    fn test_bug_sort_ord_matches_names() {
        for bug in Bug::iter_all() {
            assert_eq!(bug_sort_ord(bug), bug_name_to_sort_ord(bug.name()),
                "mismatch for {:?}", bug);
        }
    }

    #[test]
    fn test_encode_offset_empty_board() {
        assert_eq!(make_state().encode_offset(), (0, 0));
    }

    #[test]
    fn test_encode_offset_queen_centering() {
        let mut gs = make_state();
        gs.board.apply(Turn::Place(loc_to_hex((2, 3)), Bug::Queen));
        gs.board.apply(Turn::Place(loc_to_hex((3, 3)), Bug::Ant));
        assert_eq!(gs.encode_offset(), (2, 3));
    }

    #[test]
    fn test_encode_offset_centroid_before_queen() {
        let mut gs = make_state();
        gs.board.apply(Turn::Place(loc_to_hex((0, 0)), Bug::Ant));
        gs.board.apply(Turn::Place(loc_to_hex((1, 0)), Bug::Ant));
        assert_eq!(gs.encode_offset().1, 0);
    }

    #[test]
    fn test_encode_board_empty() {
        let data = make_state().encode_board_data();
        assert_eq!(data.len(), 84 * P);
        // Spatial channels all zero.
        assert!(data[..80 * P].iter().all(|&v| v == 0.0));
        // White to move.
        assert_eq!(data[80 * P], 1.0);
        // Turn 0.
        assert_eq!(data[81 * P], 0.0);
        // Queens not placed.
        assert_eq!(data[82 * P], 0.0);
        assert_eq!(data[83 * P], 0.0);
    }

    #[test]
    fn test_encode_board_single_piece() {
        let mut gs = make_state();
        gs.board.apply(Turn::Place(loc_to_hex((0, 0)), Bug::Ant));
        let data = gs.encode_board_data();

        // White ant at center (16,16): channel 3, spatial 16*32+16.
        let spatial_idx = 16 * 32 + 16;
        assert_eq!(data[3 * P + spatial_idx], 1.0);

        // Exactly one nonzero in spatial channels.
        let nonzeros = data[..80 * P].iter().filter(|&&v| v != 0.0).count();
        assert_eq!(nonzeros, 1);

        // Black to move.
        assert_eq!(data[80 * P], 0.0);
        // Turn 1/200.
        assert!((data[81 * P] - 0.005).abs() < 1e-6);
    }

    #[test]
    fn test_encode_board_queen_centering() {
        let mut gs = make_state();
        gs.board.apply(Turn::Place(loc_to_hex((2, 1)), Bug::Queen));
        gs.board.apply(Turn::Place(loc_to_hex((3, 1)), Bug::Ant));
        let data = gs.encode_board_data();

        // Queen centered at (16, 16).
        let queen_spatial = 16 * 32 + 16;
        assert_eq!(data[0 * P + queen_spatial], 1.0); // channel 0: white queen

        // Black ant at (17, 16).
        let ant_spatial = 17 * 32 + 16;
        assert_eq!(data[11 * P + ant_spatial], 1.0); // channel 8+3: black ant

        // White queen placed, black not.
        assert_eq!(data[82 * P], 1.0);
        assert_eq!(data[83 * P], 0.0);
    }

    #[test]
    fn test_search_requires_params() {
        let gs = make_state();
        assert!(run_search_raw(&gs.board, None, None).is_err());
    }

    #[test]
    fn test_search_with_depth() {
        let mut gs = make_state();
        gs.board.apply(Turn::Place(loc_to_hex((0, 0)), Bug::Queen));
        gs.board.apply(Turn::Place(loc_to_hex((1, 0)), Bug::Queen));
        let result = run_search_raw(&gs.board, Some(3), None).unwrap();
        assert_legal_move(&gs.board, result.turn);
        assert!(result.depth > 0);
    }

    #[test]
    fn test_search_with_timeout() {
        let mut gs = make_state();
        gs.board.apply(Turn::Place(loc_to_hex((0, 0)), Bug::Queen));
        gs.board.apply(Turn::Place(loc_to_hex((1, 0)), Bug::Queen));
        let result = run_search_raw(&gs.board, None, Some(100)).unwrap();
        assert_legal_move(&gs.board, result.turn);
    }

    #[test]
    fn test_search_finds_winning_move() {
        // Same position from test_minimax in eval.rs — white ant can win.
        let mut gs = make_state();
        gs.board.apply(Turn::Place(loc_to_hex((0, 0)), Bug::Queen));
        gs.board.apply(Turn::Place(loc_to_hex((1, 0)), Bug::Spider));
        gs.board.apply(Turn::Place(loc_to_hex((-1, 1)), Bug::Ant));
        gs.board.apply(Turn::Place(loc_to_hex((0, 1)), Bug::Ant));
        gs.board.apply(Turn::Place(loc_to_hex((1, 2)), Bug::Grasshopper));
        gs.board.apply(Turn::Place(loc_to_hex((1, 1)), Bug::Queen));
        gs.board.apply(Turn::Place(loc_to_hex((2, 2)), Bug::Beetle));
        gs.board.apply(Turn::Pass);
        let result = run_search_raw(&gs.board, Some(2), None).unwrap();
        // The winning move is Ant from (-1,1) to (2,1).
        assert_eq!(result.turn, Turn::Move(loc_to_hex((-1, 1)), loc_to_hex((2, 1))));
    }
}
