extern crate minimax;

use crate::cli::CliPlayer;
use crate::uhp_client::UhpPlayer;
use crate::uhp_util::UhpBoard;
use crate::{BasicEvaluator, Board, Rules};
use minimax::{
    Game, IterativeOptions, IterativeSearch, LazySmp, LazySmpOptions, MCTSOptions,
    MonteCarloTreeSearch, Move, Random, Strategy,
};
use std::time::Duration;

// A player that can play one color's moves.
pub(crate) trait Player {
    fn name(&self) -> String;
    fn new_game(&mut self, game_type: &str);
    fn play_move(&mut self, m: crate::Move);
    fn undo_move(&mut self, m: crate::Move);
    fn generate_move(&mut self) -> crate::Move;
    fn principal_variation(&self) -> Vec<crate::Move> {
        Vec::new()
    }
    fn set_max_depth(&mut self, _depth: u8) {}
    fn set_timeout(&mut self, _time: Duration) {}
}

fn face_off(
    game_type: &str, mut player1: Box<dyn Player>, mut player2: Box<dyn Player>,
) -> Option<String> {
    let mut b = UhpBoard::new(game_type);
    player1.new_game(game_type);
    player2.new_game(game_type);
    let mut players = [player1, player2];
    let mut p = 0;
    loop {
        b.inner().println();
        println!("{} ({:?}) to move", players[p].name(), b.inner().to_move());
        let m = players[p].generate_move();
        let mut moves = Vec::new();
        Rules::generate_moves(&b.inner(), &mut moves);
        if !moves.contains(&m) {
            println!("{} played an illegal move: {}", players[p].name(), b.to_move_string(m));
            println!("Game log: {}", b.game_log());
            return Some(players[1 - p].name());
        }
        b.apply(m).unwrap();
        if let Some(winner) = Rules::get_winner(&b.inner()) {
            b.inner().println();
            println!("Game log: {}", b.game_log());
            return match winner {
                minimax::Winner::Draw => None,
                minimax::Winner::PlayerJustMoved => Some(players[p].name()),
                minimax::Winner::PlayerToMove => Some(players[1 - p].name()),
            };
        }
        players[p].play_move(m);
        p = 1 - p;
        players[p].play_move(m);
    }
}

fn get_player(name: &str, config: &PlayerConfig) -> Box<dyn Player> {
    match name {
        "nokamute" => config.new_player(),
        "ai" => config.new_player(),
        "human" => Box::new(CliPlayer::new()),
        // Try to launch this as a UHP server
        _ => Box::new(UhpPlayer::new(name).unwrap()),
    }
}

pub fn play_game(config: PlayerConfig, game_type: &str, name1: &str, name2: &str) {
    let player1 = get_player(name1, &config);
    let player2 = get_player(name2, &config);
    match face_off(game_type, player1, player2) {
        None => println!("Game over: draw."),
        Some(name) => println!("Game over: {} won.", name),
    }
}

struct IterativePlayer {
    board: Board,
    strategy: IterativeSearch<BasicEvaluator>,
}

impl IterativePlayer {
    fn new(mut strategy: IterativeSearch<BasicEvaluator>) -> Self {
        strategy.set_timeout(Duration::from_secs(5));
        IterativePlayer { board: Board::default(), strategy }
    }
}

impl Player for IterativePlayer {
    fn name(&self) -> String {
        "nokamute".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.board = Board::new_from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        m.apply(&mut self.board);
    }

    fn undo_move(&mut self, m: crate::Move) {
        m.undo(&mut self.board);
    }

    fn generate_move(&mut self) -> crate::Move {
        self.strategy.choose_move(&self.board).unwrap()
    }

    fn principal_variation(&self) -> Vec<crate::Move> {
        self.strategy.principal_variation().to_vec()
    }

    fn set_max_depth(&mut self, depth: u8) {
        self.strategy.set_max_depth(depth as usize);
    }

    fn set_timeout(&mut self, time: Duration) {
        self.strategy.set_timeout(time);
    }
}

struct LazySmpPlayer {
    board: Board,
    strategy: LazySmp<BasicEvaluator>,
}

impl LazySmpPlayer {
    fn new(mut strategy: LazySmp<BasicEvaluator>) -> Self {
        strategy.set_timeout(Duration::from_secs(5));
        LazySmpPlayer { board: Board::default(), strategy }
    }
}

impl Player for LazySmpPlayer {
    fn name(&self) -> String {
        "nokamute".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.board = Board::new_from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        m.apply(&mut self.board);
    }

    fn undo_move(&mut self, m: crate::Move) {
        m.undo(&mut self.board);
    }

    fn generate_move(&mut self) -> crate::Move {
        self.strategy.choose_move(&self.board).unwrap()
    }

    fn principal_variation(&self) -> Vec<crate::Move> {
        self.strategy.principal_variation().to_vec()
    }

    fn set_max_depth(&mut self, depth: u8) {
        self.strategy.set_max_depth(depth as usize);
    }

    fn set_timeout(&mut self, time: Duration) {
        self.strategy.set_timeout(time);
    }
}

struct MctsPlayer {
    board: Board,
    strategy: MonteCarloTreeSearch,
}

impl MctsPlayer {
    fn new(opts: MCTSOptions) -> Self {
        let mut strategy = MonteCarloTreeSearch::new(opts);
        strategy.set_timeout(Duration::from_secs(5));
        MctsPlayer { board: Board::default(), strategy }
    }
}

impl Player for MctsPlayer {
    fn name(&self) -> String {
        "nokamute-mcts".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.board = Board::new_from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        m.apply(&mut self.board);
    }

    fn undo_move(&mut self, m: crate::Move) {
        m.undo(&mut self.board);
    }

    fn generate_move(&mut self) -> crate::Move {
        (&mut self.strategy as &mut dyn Strategy<Rules>).choose_move(&self.board).unwrap()
    }

    fn set_timeout(&mut self, time: Duration) {
        self.strategy.set_timeout(time);
    }
}

#[derive(Default)]
struct RandomPlayer {
    board: Board,
    strategy: Random,
}

impl Player for RandomPlayer {
    fn name(&self) -> String {
        "random".to_owned()
    }

    fn new_game(&mut self, game_type: &str) {
        self.board = Board::new_from_game_type(game_type).unwrap();
    }

    fn play_move(&mut self, m: crate::Move) {
        m.apply(&mut self.board);
    }

    fn undo_move(&mut self, m: crate::Move) {
        m.undo(&mut self.board);
    }

    fn generate_move(&mut self) -> crate::Move {
        (&mut self.strategy as &mut dyn Strategy<Rules>).choose_move(&self.board).unwrap()
    }
}

fn exit(msg: String) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1)
}

pub(crate) enum PlayerStrategy {
    Iterative(LazySmpOptions),
    Random,
    Mcts(MCTSOptions),
}

pub struct PlayerConfig {
    pub(crate) num_threads: Option<u32>,
    pub(crate) opts: IterativeOptions,
    pub(crate) strategy: PlayerStrategy,
    pub(crate) eval: BasicEvaluator,
}

pub fn configure_player() -> Result<(PlayerConfig, Vec<String>), pico_args::Error> {
    let mut args = pico_args::Arguments::from_env();

    let mut config = PlayerConfig::new();

    // Configure common minimax options.
    if args.contains(["-v", "--verbose"]) {
        config.opts = config.opts.verbose();
    }
    let table_size: Option<usize> = args.opt_value_from_str("--table_mb")?;
    config.opts =
        config.opts.with_table_byte_size(table_size.unwrap_or(100).checked_shl(20).unwrap());
    let window_arg: Option<u32> = args.opt_value_from_str("--aspiration-window")?;
    if let Some(window) = window_arg {
        config.opts = config.opts.with_aspiration_window(window as minimax::Evaluation);
    }
    if args.contains("--double-step") {
        config.opts = config.opts.with_double_step_increment();
    }
    if args.contains("--null-move-pruning") {
        config.opts = config.opts.with_null_move_depth(3);
    }

    // 0 for num_cpu threads; >0 for specific count.
    config.num_threads = args.opt_value_from_str("--num-threads")?.map(|thread_arg: String| {
        if thread_arg == "max" || thread_arg == "all" {
            0
        } else if let Ok(num) = thread_arg.parse::<u32>() {
            num
        } else {
            exit(format!("Could not parse num_threads={}. Expected int or 'max'", thread_arg));
        }
    });

    // Configure specific strategy.
    let strategy: Option<String> = args.opt_value_from_str("--strategy")?;
    config.strategy = match strategy.as_ref().map(String::as_str).unwrap_or("iterative") {
        "random" => PlayerStrategy::Random,
        "mcts" => PlayerStrategy::Mcts(MCTSOptions::default().with_max_rollout_depth(200)),
        "mtdf" => {
            config.opts = config.opts.with_mtdf();
            PlayerStrategy::Iterative(LazySmpOptions::new())
        }
        "iterative" => {
            let mut smp_opts = LazySmpOptions::new();
            if args.contains("--differing-depths") {
                smp_opts = smp_opts.with_differing_depths();
            }
            PlayerStrategy::Iterative(smp_opts)
        }
        _ => exit(format!("Unrecognized strategy: {}", strategy.unwrap_or_default())),
    };
    Ok((config, args.finish().into_iter().map(|s| s.into_string().unwrap()).collect::<Vec<_>>()))
}

impl PlayerConfig {
    fn new() -> Self {
        Self {
            num_threads: None,
            opts: IterativeOptions::new(),
            strategy: PlayerStrategy::Iterative(LazySmpOptions::new()),
            eval: BasicEvaluator::default(),
        }
    }

    pub(crate) fn new_player(&self) -> Box<dyn Player> {
        match &self.strategy {
            PlayerStrategy::Random => Box::new(RandomPlayer::default()),
            PlayerStrategy::Mcts(opts) => {
                let mut opts = opts.clone();
                let num_threads = self.num_threads.unwrap_or(0);
                if num_threads > 0 {
                    opts = opts.with_num_threads(num_threads);
                }
                Box::new(MctsPlayer::new(opts))
            }
            PlayerStrategy::Iterative(smp_opts) => {
                let num_threads = self.num_threads.unwrap_or(1);
                if num_threads == 1 {
                    Box::new(IterativePlayer::new(IterativeSearch::new(
                        self.eval.clone(),
                        self.opts,
                    )))
                } else {
                    let mut smp_opts = smp_opts.clone();
                    if num_threads > 0 {
                        smp_opts = smp_opts.with_num_threads(num_threads as usize);
                    }
                    Box::new(LazySmpPlayer::new(LazySmp::new(
                        self.eval.clone(),
                        self.opts,
                        smp_opts,
                    )))
                }
            }
        }
    }
}
