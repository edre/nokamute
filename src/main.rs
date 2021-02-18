use hive;

fn main() {
    for arg in std::env::args() {
        if arg.starts_with("--perft") {
            let game_type = arg.split('=').skip(1).next().unwrap_or("Base");
            hive::perft(game_type);
            return;
        }
    }
    hive::terminal_game_interface();
}
