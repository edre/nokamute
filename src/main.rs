use nokamute::*;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect::<Vec<_>>();
    match args.get(0).unwrap_or(&"cli".to_owned()).as_ref() {
        "cli" => {
            terminal_game_interface();
        }
        "uhp" => {
            UhpServer::new().serve();
        }
        "play" => {
            let game_type = args.get(1).map(|s| s.as_ref()).unwrap_or("Base+MLP");
            let player1 = args.get(2).map(|s| s.as_ref()).unwrap_or("human");
            let player2 = args.get(3).map(|s| s.as_ref()).unwrap_or("ai");
            play_game(game_type, player1, player2);
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
            perft_debug(&args[1..], "Base", 8);
        }
        _ => {
            println!("commands are: cli, perft, perft-cheating, perft-debug, play, uhp");
        }
    }
}
