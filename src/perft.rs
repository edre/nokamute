use crate::uhp_client::UhpClient;
use crate::{Board, Rules, Turn};
use minimax::{Game, Strategy};

fn standard_games(game_string: &str) -> &str {
    match game_string {
	"beetle_gate" => "Base+MLP;InProgress;White[5];wB1;bB1 -wB1;wQ wB1-;bQ /bB1;wB2 wQ-;bQ /wB1;wB2 wQ;bB1 bQ",
	_ => game_string,
    }
}

pub fn perft_single_thread(game_string: &str) {
    let game_string = standard_games(game_string);
    println!("{}", game_string);
    let mut b = Board::from_game_string(game_string).unwrap();
    if game_string.contains(';') {
        b.println();
    }
    minimax::perft::<Rules>(&mut b, 20, false);
}

pub fn perft_multi_thread(game_string: &str) {
    let game_string = standard_games(game_string);
    println!("{}", game_string);
    let mut b = Board::from_game_string(game_string).unwrap();
    if game_string.contains(';') {
        b.println();
    }
    minimax::perft::<Rules>(&mut b, 20, true);
}

pub fn uhp_tests(engine_cmd: &[String]) {
    const FAILED: &'static str = "\x1b[31mFAILED\x1b[m";
    const PASSED: &'static str = "\x1b[32mpassed\x1b[m";
    let mut engine = UhpClient::new(engine_cmd).unwrap();
    // TODO: respect capabilities
    let lines = std::include_str!("../data/uhp_tests.txt").split('\n').collect::<Vec<_>>();
    let mut i = 0;
    let mut name = "";
    while i < lines.len() {
        if lines[i].is_empty() {
            i += 1;
            continue;
        }
        if lines[i].starts_with("# ") {
            name = &lines[i][2..];
            i += 1;
            continue;
        }

        print!("Test {}... ", name);
        let game_state_string = lines[i];
        let expected_moves_string = lines[i + 1];
        i += 2;

        if let Err(error) = engine.new_game(game_state_string) {
            println!("{} newgame: {:?}", FAILED, error);
            continue;
        }
        let movestrings = match engine.raw_generate_moves() {
            Ok(s) => s,
            Err(error) => {
                println!("{} validmoves: {:?}", FAILED, error);
                continue;
            }
        };
        // TODO: actually compare moves
        if expected_moves_string.split(';').count() != movestrings.split(';').count() {
            // TODO: verbose mode: dump difference
            println!("{}", FAILED);
            continue;
        }

        println!("{}", PASSED);
    }
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
            println!("iter {}", iter);
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
                    "iter {} game end disagreement: board_winner={:?} engine_winner={:?}",
                    iter, board_winner, engine_winner
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
        "iteration {} found discrepancy: {} vs {} moves",
        iter,
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
            println!("{}:", title);
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
