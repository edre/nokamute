use crate::uhp_client::UhpClient;
use crate::{Board, Rules, Turn, UhpError};
use minimax::{Game, Strategy};

fn standard_games(game_string: &str) -> &str {
    match game_string {
	"beetle_gate" => "Base+MLP;InProgress;White[5];wB1;bB1 -wB1;wQ wB1-;bQ /bB1;wB2 wQ-;bQ /wB1;wB2 wQ;bB1 bQ",
	_ => game_string,
    }
}

pub fn perft(game_string: &str, parallel: bool) {
    let game_string = standard_games(game_string);
    println!("{game_string}");
    let mut b = Board::from_game_string(game_string).unwrap();
    if game_string.contains(';') {
        b.println();
    }
    minimax::perft::<Rules>(&mut b, 20, parallel);
}

pub fn uhp_tests(engine_cmd: &[String]) -> bool {
    uhp_tests_with_verbosity(engine_cmd, false)
}

pub fn uhp_tests_with_verbosity(engine_cmd: &[String], verbose: bool) -> bool {
    const FAILED: &str = "\x1b[31mFAILED\x1b[m";
    const PASSED: &str = "\x1b[32mpassed\x1b[m";
    let mut engine = UhpClient::new(engine_cmd).unwrap();
    println!("UHP testsuite for {}", engine.name);

    let lines = std::include_str!("../data/uhp_tests.txt").split('\n').collect::<Vec<_>>();
    let mut i = 0;
    let mut name = "";
    let mut success = true;
    'testcases: while i < lines.len() {
        if lines[i].is_empty() {
            i += 1;
            continue;
        }
        if lines[i].starts_with("# ") {
            name = &lines[i][2..];
            i += 1;
            continue;
        }

        print!("Test {name}... ");
        let game_state_string = lines[i];
        let expected_moves_string = lines[i + 1];
        i += 2;

        if !engine.capable_of_game_string(game_state_string) {
            println!("skipped");
            continue;
        }

        let mut groups = game_state_string.split(';');
        let game_type = groups.next().unwrap();
        if let Err(error) = engine.new_game(game_type) {
            println!("{FAILED} newgame: {error:?}");
            if verbose {
                println!("  testcase: {game_state_string}");
            }
            success = false;
            continue;
        }
        // Skip to moves.
        groups.next();
        groups.next();

        let mut new_state = "Base;NotStarted".to_string();
        let mut played_moves = Vec::new();
        for move_string in groups {
            match engine.raw_play(move_string) {
                Ok(string) => {
                    played_moves.push(move_string);
                    new_state = string;
                }
                Err(UhpError::EngineError(error)) => {
                    println!("{FAILED} play {move_string} failed: {error}");
                    if verbose {
                        println!("  testcase: {game_state_string}");
                        if !played_moves.is_empty() {
                            println!("  played: {}", played_moves.join(";"));
                            println!("  game log: {}", engine.game_log());
                        }
                    }
                    success = false;
                    continue 'testcases;
                }
                // Skip other errors from nokamute not parsing the move.
                _ => {}
            }
        }

        let expected_state = game_state_string.split(';').nth(1).unwrap_or("not found");
        let state = new_state.split(';').nth(1).unwrap_or("not found");
        if expected_state != state {
            println!("{FAILED} end state expected {expected_state} found {state}");
            if verbose {
                println!("  testcase: {game_state_string}");
                println!("  engine state: {new_state}");
            }
            success = false;
            continue;
        }
        if expected_state != "InProgress" {
            println!("{PASSED}");
            continue;
        }

        let movestrings = match engine.raw_generate_moves() {
            Ok(s) => s,
            Err(error) => {
                println!("{FAILED} validmoves: {error:?}");
                if verbose {
                    println!("  testcase: {game_state_string}");
                    println!("  game log: {}", engine.game_log());
                }
                success = false;
                continue;
            }
        };

        let expected_moves =
            expected_moves_string.split(';').filter(|s| !s.trim().is_empty()).collect::<Vec<_>>();
        let engine_moves =
            movestrings.split(';').filter(|s| !s.trim().is_empty()).collect::<Vec<_>>();

        let expected_count = expected_moves.len();
        let count = engine_moves.len();
        // Different reference pieces can describe the same UHP move, so compare parsed turns.
        let board = match Board::from_game_string(game_state_string) {
            Ok(board) => board,
            Err(error) => {
                println!("{FAILED} invalid testcase: {error:?}");
                success = false;
                continue;
            }
        };
        let difference = semantic_move_diff(&board, &expected_moves, &engine_moves);
        if !difference.is_empty() {
            if expected_count != count {
                println!("{FAILED} expected {expected_count} moves, found {count}");
            } else {
                println!("{FAILED} valid moves differ ({count} moves each)");
            }
            if verbose {
                println!("  testcase: {game_state_string}");
                board.println();
                if !difference.missing.is_empty() {
                    println!("  missing moves: {}", difference.missing.join(";"));
                }
                if !difference.extra.is_empty() {
                    println!("  extra moves: {}", difference.extra.join(";"));
                }
                if !difference.invalid_expected.is_empty() {
                    println!("  invalid expected moves: {}", difference.invalid_expected.join(";"));
                }
                if !difference.invalid_actual.is_empty() {
                    println!("  invalid engine moves: {}", difference.invalid_actual.join(";"));
                }
                println!("  expected moves: {expected_moves_string}");
                println!("  engine moves: {movestrings}");
            }
            success = false;
            continue;
        }

        println!("{PASSED}");
    }
    success
}

#[derive(Debug, Default, Eq, PartialEq)]
struct MoveDifference<'a> {
    missing: Vec<&'a str>,
    extra: Vec<&'a str>,
    invalid_expected: Vec<&'a str>,
    invalid_actual: Vec<&'a str>,
}

impl MoveDifference<'_> {
    fn is_empty(&self) -> bool {
        self.missing.is_empty()
            && self.extra.is_empty()
            && self.invalid_expected.is_empty()
            && self.invalid_actual.is_empty()
    }
}

fn semantic_move_diff<'a>(
    board: &Board, expected: &[&'a str], actual: &[&'a str],
) -> MoveDifference<'a> {
    use std::collections::BTreeMap;

    fn group_moves<'a>(
        board: &Board, moves: &[&'a str],
    ) -> (BTreeMap<Turn, Vec<&'a str>>, Vec<&'a str>) {
        let mut grouped = BTreeMap::<Turn, Vec<&str>>::new();
        let mut invalid = Vec::new();
        for &move_string in moves {
            match parse_comparable_move(board, move_string) {
                Ok(turn) => grouped.entry(turn).or_default().push(move_string),
                Err(_) => invalid.push(move_string),
            }
        }
        (grouped, invalid)
    }

    // Keep each original string so diagnostics reproduce the exact notation after matching turns.
    let (expected_by_turn, invalid_expected) = group_moves(board, expected);
    let (mut actual_by_turn, invalid_actual) = group_moves(board, actual);
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

fn parse_comparable_move(board: &Board, move_string: &str) -> Result<Turn, UhpError> {
    let move_string = move_string.trim();
    let turn = board.from_move_string(move_string)?;
    if let Turn::Move(_, _) = turn {
        let source = move_string.split_once(' ').map(|(source, _)| source);
        let canonical = board.to_move_string(turn);
        let canonical_source = canonical.split_once(' ').map(|(source, _)| source);
        // Pillbug throws may move an opponent's piece, but the named source must still be on top.
        if source != canonical_source {
            return Err(UhpError::InvalidMove(move_string.to_owned()));
        }
    }
    Ok(turn)
}

pub fn perft_debug(engine_cmd: &[String], game_string: &str, depth: usize) {
    println!("\nExploring random games to compare with nokamute's move generator...");
    let game_string = standard_games(game_string);
    let mut engine = UhpClient::new(engine_cmd).unwrap();
    engine.new_game(game_string).unwrap();
    let mut board = Board::from_game_string(game_string).unwrap();
    // Generate random positions at the given depth, and compare output.
    let mut rand = minimax::Random::<Rules>::new();
    let mut moves = Vec::new();
    for iter in 0.. {
        if iter % 100 == 0 {
            println!("iter {iter}");
        }
        // Roll out a random game to the desired depth.
        let mut stack = Vec::new();
        for _ in 0..depth {
            // Check for discrepancies on every move.
            moves.clear();
            Rules::generate_moves(&board, &mut moves);
            let engine_moves = engine.generate_moves().unwrap();
            if moves.len() != engine_moves.len() {
                println!("game log: {}", engine.game_log());
                dump_difference(&mut board, iter, &moves, &engine_moves);
                return;
            }

            let m = rand.choose_move(&board).unwrap();
            stack.push(m);
            board.apply(m);
            let board_winner = Rules::get_winner(&board);
            let engine_winner = engine.apply(m).unwrap();
            if board_winner != engine_winner {
                println!(
                    "iter {iter} game end disagreement: board_winner={board_winner:?} engine_winner={engine_winner:?}"
                );
                println!("game log: {}", engine.game_log());
                board.println();
                return;
            }
            if board_winner.is_some() {
                break;
            }
        }

        // Unwrap
        engine.undo(stack.len()).unwrap();
        while let Some(m) = stack.pop() {
            board.undo(m);
        }
    }
}

fn dump_difference(board: &mut Board, iter: usize, nokamute_moves: &[Turn], engine_moves: &[Turn]) {
    println!(
        "iteration {iter} found discrepancy: {} vs {} moves",
        nokamute_moves.len(),
        engine_moves.len()
    );
    println!("position:");
    board.println();
    let mut common = Vec::new();
    let mut nokamute_only = Vec::new();
    let mut engine_only = Vec::new();
    for &m in nokamute_moves.iter() {
        if engine_moves.contains(&m) {
            common.push(m);
        } else {
            nokamute_only.push(m);
        }
    }
    for &m in engine_moves.iter() {
        if !nokamute_moves.contains(&m) {
            engine_only.push(m);
        }
    }

    let nokamute_dups = find_dups(nokamute_moves);
    let engine_dups = find_dups(engine_moves);

    let mut print_moves = |title: &str, moves: &[Turn]| {
        if !moves.is_empty() {
            println!("{title}:");
        }
        for &m in moves.iter() {
            board.apply(m);
            board.println();
            board.undo(m);
        }
    };

    print_moves("nokamute only moves", &nokamute_only);
    print_moves("UHP engine only moves", &engine_only);
    print_moves("nokamute duplicate moves", &nokamute_dups);
    print_moves("engine duplicate moves", &engine_dups);
}

fn find_dups(moves: &[Turn]) -> Vec<Turn> {
    let mut dups = Vec::new();
    for &m in moves.iter() {
        if moves.iter().filter(|&&m2| m == m2).count() > 1 && !dups.contains(&m) {
            dups.push(m);
        }
    }
    dups
}

#[test]
fn test_perft() {
    let mut b = Board::from_game_type("Base").unwrap();
    let move_counts = minimax::perft::<Rules>(&mut b, 4, false);
    assert_eq!(move_counts, vec![1, 4, 96, 1440, 21600]);

    b = Board::from_game_type("Base+MLP").unwrap();
    let move_counts = minimax::perft::<Rules>(&mut b, 4, false);
    assert_eq!(move_counts, vec![1, 7, 294, 6678, 151686]);
}

#[test]
fn test_uhp_tests_public_signature() {
    let _: fn(&[String]) -> bool = uhp_tests;
}

#[test]
fn test_semantic_move_diff_accepts_equivalent_references() {
    let board =
        Board::from_game_string("Base;InProgress;White[3];wG1;bG1 wG1-;wQ -wG1;bQ bG1-").unwrap();
    let difference = semantic_move_diff(&board, &[r"wA1 \wG1"], &["wA1 wQ/"]);
    assert!(difference.is_empty(), "{difference:?}");
}

#[test]
fn test_semantic_move_diff_finds_equal_count_mismatch() {
    let board =
        Board::from_game_string("Base;InProgress;White[3];wG1;bG1 wG1-;wQ -wG1;bQ bG1-").unwrap();
    let difference = semantic_move_diff(&board, &[r"wA1 \wG1"], &["wA1 -wQ"]);
    assert_eq!(difference.missing, vec![r"wA1 \wG1"]);
    assert_eq!(difference.extra, vec!["wA1 -wQ"]);
}

#[test]
fn test_semantic_move_diff_finds_duplicates_and_invalid_moves() {
    let board =
        Board::from_game_string("Base;InProgress;White[3];wG1;bG1 wG1-;wQ -wG1;bQ bG1-").unwrap();
    let difference = semantic_move_diff(
        &board,
        &[r"wA1 \wG1", "invalid-fixture-move"],
        &[r"wA1 \wG1", "wA1 wQ/", "invalid-engine-move"],
    );
    assert_eq!(difference.extra, vec!["wA1 wQ/"]);
    assert_eq!(difference.invalid_expected, vec!["invalid-fixture-move"]);
    assert_eq!(difference.invalid_actual, vec!["invalid-engine-move"]);
}

#[test]
fn test_semantic_move_diff_rejects_covered_sources() {
    let board = Board::from_game_string(
        r"Base;InProgress;White[10];wG1;bG1 wG1-;wQ /wG1;bQ bG1-;wS1 wQ\;bA1 bQ-;wB1 /wS1;bA1 -wQ;wB1 wS1\;bA2 bQ-;wB1 /wS1;bA2 wG1\;wB1 wS1\;bA3 bQ-;wB1 /wS1;bS1 bQ\;wB1 wS1;bS1 wB1\",
    )
    .unwrap();
    let legal = "wB1 /wB1";
    let covered = "wS1 /wB1";
    assert_eq!(board.from_move_string(legal).unwrap(), board.from_move_string(covered).unwrap());

    let difference = semantic_move_diff(&board, &[legal], &[covered]);
    assert_eq!(difference.missing, vec![legal]);
    assert_eq!(difference.invalid_actual, vec![covered]);
}

// Regression suite for bugs caught by perft-debug.

#[test]
fn test_winner_fail() {
    use minimax::Game;
    let b = Board::from_game_string(r#"Base+MLP;InProgress;Black[99];wP;bB1 \wP;wG1 wP-;bA1 -bB1;wL /wP;bP /bA1;wQ wP\;bQ -bA1;wQ \wG1;bG1 /bP;wL \bB1;bB2 bP\;wQ bA1\;bL -bQ;wL bB1/;wQ bL\;wG1 /wP"#).unwrap();
    assert_eq!(None, Rules::get_winner(&b));
}

#[test]
fn test_mosquito_throw() {
    // TODO: fix
    //let b = UhpBoard::new(r#"Base+MLP;InProgress;wM;bP \wM;wS1 /wM;bB1 bP/;wB1 -wS1;bM \bB1;wQ wM\;bQ /bM;wG1 /wB1;bG1 -bM;wP -wG1;bM bB1;wQ /bP;bG2 bG1/;wP wP\;bB2 -bG1;wB2 wB1\;wQ bM\;wA1 -wB1;bA1 bM/"#);
    // assert the mosquito can throw the pillbug
    // another: r#"Base+MLP;wM;bB1 /wM;wP wM/;bG1 bB1\;wG1 wP/;bS1 bG1\;wQ wG1\;bQ bS1-;wG1 wP\;bM /bB1;wA1 wG1\;bM \wQ;wP -bM;bL /bG1;wP wP\;bS2 bS1\;wB1 wA1-;wQ -bM"#
    // Assert pillbug can throw mosquito
}

#[test]
fn test_spider_walk() {
    // game log: wL;bP wL-;wM \wL;bL bP\;wP -wM;bM bP/;wQ wP\;bQ bL-;wA1 wM/;bG1 bM-;wS1 wA1/;bG1 wA1\;wS2 \wP;bG1 bM\;wA2 /wS2;bA1 bM/;wA3 -wS1;bA2 bQ\;wA3 bA1\;bG2 /bA2;wA2 bA1/;bG1 wA1\;wG1 /wQ;wL wL\;wG2 /wS2;bG3 bA2-;wG3 -wG2;bA3 /bG2;wA2 wL\;bG3 bL\;wA3 bG2\;bA3 bA1\;wA3 wQ\;bS1 bM\;wA2 bA3\;bA3 -wG1;wB1 -wG3;bA3 bA1\;wA3 wB1\;bP wS1\;wA1 wG2\;bA2 -wB1;wA2 bA1/;bA2 -wS2;wA2 /wA1;bA2 /wL;wA3 \wS1;bA2 wB1\;wQ /wS1;bA3 wA3/;wL bA1\;bA1 \bA1;wA2 bQ-;bA2 \wG3;wA2 /wA1;bA2 -bL;wL bG1\;bA3 bG2\;wS2 -wQ;bA1 -wA2;wL bQ\;bA1 wA3/;wM -wB1;bA3 /bG3;wB2 \wG3;bS2 bG1\;wA2 wG3\;bA1 wS2\;wS2 \wA3;bA2 bS1/;wL bG2\;bA3 /wA3;wA2 bA2-;bA3 wG1\;wA2 /bG3;bA2 \wP;wA2 wB2/;bM bQ\;wS2 bP/;bL /bL;bA2 wP\;bB1 bL\;wA3 -bL;bA2 bA1\;wA1 wB1\;bG1 -wS1;wA1 /bG1;bG1 wQ\;wA1 /wM;bB2 bS2\;wA2 wG2\;wQ wS2\
    // Assert that the top spider gets normal moves generated.
}
