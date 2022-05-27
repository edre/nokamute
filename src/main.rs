use nokamute::*;

fn help() {
    println!(
        r#"nokamute hive engine {}

commands:
 cli:   Interactive interface to a board
 uhp:   Run as a Universal Hive Protocol engine
 play [game_type] [player1] [player2]:
        Play a game, with each player being "human", "ai|nokamute",
        or a path to a UHP engine
 perft [game_state]:
        Count the number of board states at each depth
 perft-cheating [game_state]:
        Perft, but with multiple threads to get the answer sooner
 perft-debug game_state depth engine_command
        Find discrepancies between nokamute and another UHP engine
        from the specified starting position at the specified depth

engine flags:
 --verbose
 --strategy=iterative|ybw|mcts|mtdf|random
 --table-mb=[int]
 --num-threads=[int]|all
 --aspiration-window=[int]
 --double-step
 --quiet-search
 --null-move-pruning"#,
        env!("CARGO_PKG_VERSION")
    );
}

fn main() {
    let (config, args) = configure_player().unwrap();
    match args.get(0).unwrap_or(&"uhp".to_owned()).as_ref() {
        "cli" => {
            terminal_game_interface(config);
        }
        "uhp" => {
            UhpServer::new(config).serve();
        }
        "play" => {
            let game_type = args.get(1).map(|s| s.as_ref()).unwrap_or("Base+MLP");
            let player1 = args.get(2).map(|s| s.as_ref()).unwrap_or("human");
            let player2 = args.get(3).map(|s| s.as_ref()).unwrap_or("ai");
            play_game(config, game_type, player1, player2);
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
        "perft-debug" => {
            if args.len() < 4 {
                println!("perft-debug requires game_type, depth, and engine command");
                return;
            }
            let depth = if let Ok(i) = args[2].parse::<usize>() {
                i
            } else {
                println!("perft-debug depth must be an integer");
                return;
            };
            let game_type = &args[1];
            perft_debug(&args[3..], game_type, depth);
        }
        _ => {
            help();
        }
    }
}
