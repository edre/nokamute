#[cfg(not(target_arch = "wasm32"))]
use nokamute::*;
#[cfg(not(target_arch = "wasm32"))]
use std::ffi::OsString;

#[cfg(not(target_arch = "wasm32"))]
fn help() {
    println!(
        r#"nokamute hive engine {}

commands:
 cli:   Interactive interface to a board
 uhp:   Run as a Universal Hive Protocol engine
 play [--game-type=] [--depth=] [--timeout=] [player1] [player2]:
        Play a game, with each player being "human", "ai|nokamute",
        or a path to a UHP engine
 perft [game_state]:
        Count the number of board states at each depth
 perft-cheating [game_state]:
        Perft, but with multiple threads to get the answer sooner
 uhp-debug engine_command
        Run external UHP engine through correctness testsuite,
        then randomly search for discrepancies with nokamute's move generator.

engine flags:
 --verbose
 --strategy=iterative|mcts|mtdf|random
 --table-mb=[int]
 --num-threads=[int]|all
 --aspiration-window=[int]
 --double-step
 --quiet-search
 --null-move-pruning"#,
        env!("CARGO_PKG_VERSION")
    );
}

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    let (config, args) = configure_player().unwrap();
    match args.get(0).unwrap_or(&"uhp".to_owned()).as_ref() {
        "cli" => {
            terminal_game_interface(config);
        }
        "uhp" => {
            uhp_serve(config);
        }
        "play" => {
            let mut args = pico_args::Arguments::from_vec(
                args.iter().map(|s| s.into()).collect::<Vec<OsString>>(),
            );
            let game_type = args
                .opt_value_from_str("--game-type")
                .unwrap()
                .unwrap_or_else(|| "Base+MLP".to_owned());
            let depth: Option<u8> = args.opt_value_from_str("--depth").unwrap();
            let timeout: Option<String> = args.opt_value_from_str("--timeout").unwrap();
            let args =
                args.finish().into_iter().map(|s| s.into_string().unwrap()).collect::<Vec<_>>();

            let player1 = args.get(1).map(|s| s.as_ref()).unwrap_or("human");
            let player2 = args.get(2).map(|s| s.as_ref()).unwrap_or("ai");
            play_game(config, &game_type, player1, player2, depth, timeout);
        }
        "perft" => {
            // For engine performance comparisons.
            let game_type = args.get(1).map(|s| s.as_ref()).unwrap_or("Base");
            perft_single_thread(game_type);
        }
        "perft-cheating" => {
            // For more quickly getting values for correctness checking.
            let game_type = args.get(1).map(|s| s.as_ref()).unwrap_or("Base");
            perft_multi_thread(game_type);
        }
        "uhp-debug" => {
            if args.len() < 2 {
                println!("uhp-debug requires engine command");
                return;
            }
            uhp_tests(&args[1..]);
            perft_debug(&args[1..], "Base", 20);
        }
        _ => {
            help();
        }
    }
}
