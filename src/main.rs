use nokamute::*;

fn main() {
    let args: Vec<String> = std::env::args().collect::<Vec<_>>();
    match args.get(1).unwrap_or(&"cli".to_owned()).as_ref() {
        "cli" => {
            terminal_game_interface();
        }
        "perft" => {
            let game_type = args.get(2).map(|s| s.as_ref()).unwrap_or("Base");
            perft(game_type);
        }
        "perft-debug" => {
            perft_debug(&args[2..], "Base", 8);
        }
        _ => {
            println!("commands are: cli, perft, perft-debug");
        }
    }
}
