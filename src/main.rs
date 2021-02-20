use nokamute::*;

fn main() {
    for arg in std::env::args() {
        if arg.starts_with("--perft") {
            let game_type = arg.split('=').skip(1).next().unwrap_or("Base");
            perft(game_type);
            return;
        }
    }
    terminal_game_interface();
}
