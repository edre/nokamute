extern crate minimax;

#[cfg(not(target_arch = "wasm32"))]
use crate::cli::CliPlayer;
#[cfg(not(target_arch = "wasm32"))]
use crate::mcts::BiasedRollouts;
#[cfg(not(target_arch = "wasm32"))]
use crate::uhp_client::UhpPlayer;
use crate::{nokamute_version, BasicEvaluator, Board, Bug, Rules, Turn};
use minimax::*;
use std::time::Duration;

// A player that can play one color's moves.
pub(crate) trait Player {
    fn name(&self) -> String;
    fn new_game(&mut self, game_type: &str);
    fn play_move(&mut self, m: Turn);
    fn undo_move(&mut self, m: Turn);
    fn generate_move(&mut self) -> Turn;
    fn principal_variation(&self) -> Vec<Turn> {
        Vec::new()
    }
    fn set_max_depth(&mut self, _depth: u8) {}
    fn set_timeout(&mut self, _time: Duration) {}
}

#[cfg(not(target_arch = "wasm32"))]
fn face_off(
    game_type: &str, mut player1: Box<dyn Player>, mut player2: Box<dyn Player>,
) -> Option<String> {
    let mut b = Board::from_game_type(game_type).unwrap();
    player1.new_game(game_type);
    player2.new_game(game_type);
    let mut players = [player1, player2];
    let mut p = 0;
    loop {
        b.println();
        println!("{} ({:?}) to move", players[p].name(), b.to_move());
        let m = players[p].generate_move();
        let mut moves = Vec::new();
        Rules::generate_moves(&b, &mut moves);
        if !moves.contains(&m) {
            println!("{} played an illegal move: {}", players[p].name(), b.to_move_string(m));
            println!("Game log: {}", b.game_log());
            return Some(players[1 - p].name());
        }
        b.apply(m);
        if let Some(winner) = Rules::get_winner(&b) {
            b.println();
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

#[cfg(not(target_arch = "wasm32"))]
fn get_player(name: &str, config: &PlayerConfig) -> Box<dyn Player> {
    match name {
        "nokamute" => config.new_player(),
        "ai" => config.new_player(),
        "human" => Box::new(CliPlayer::new()),
        // Try to launch this as a UHP server
        _ => Box::new(UhpPlayer::new(name).unwrap()),
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn play_game(
    config: PlayerConfig, game_type: &str, name1: &str, name2: &str, depth: Option<u8>,
    timeout: Option<String>,
) {
    let mut player1 = get_player(name1, &config);
    let mut player2 = get_player(name2, &config);
    if let Some(depth) = depth {
        player1.set_max_depth(depth);
        player2.set_max_depth(depth);
    } else if let Some(input) = timeout {
        let timeout = if input.ends_with('s') {
            input[..input.len() - 1].parse::<u64>().map(Duration::from_secs)
        } else if input.ends_with('m') {
            input[..input.len() - 1].parse::<u64>().map(|m| Duration::from_secs(m * 60))
        } else {
            exit("Could not parse --timeout (add units)".to_string());
        }
        .unwrap_or_else(|_| exit("Could not parse --timeout (add units)".to_string()));
        player1.set_timeout(timeout);
        player2.set_timeout(timeout);
    }
    match face_off(game_type, player1, player2) {
        None => println!("Game over: draw."),
        Some(name) => println!("Game over: {} won.", name),
    }
}

struct NokamutePlayer {
    board: Board,
    strategy: Box<dyn Strategy<Rules> + Send>,
    random_opening: bool,
    name: String,
}

impl NokamutePlayer {
    fn new(strategy: Box<dyn Strategy<Rules> + Send>, random_opening: bool) -> Self {
        Self::new_with_name(&format!("nokamute {}", nokamute_version()), strategy, random_opening)
    }

    fn new_with_name(
        name: &str, mut strategy: Box<dyn Strategy<Rules> + Send>, random_opening: bool,
    ) -> Self {
        strategy.set_timeout(Duration::from_secs(5));
        NokamutePlayer { board: Board::default(), strategy, random_opening, name: name.to_owned() }
    }
}

impl Player for NokamutePlayer {
    fn name(&self) -> String {
        self.name.clone()
    }

    fn new_game(&mut self, game_string: &str) {
        self.board = Board::from_game_string(game_string).unwrap();
    }

    fn play_move(&mut self, m: Turn) {
        self.board.apply(m);
    }

    fn undo_move(&mut self, m: Turn) {
        self.board.undo(m);
    }

    fn generate_move(&mut self) -> Turn {
        if self.random_opening {
            // Ignore minimax and just throw out a random jumpy bug for the first move.
            if self.board.turn_num < 2 {
                loop {
                    let turn =
                        minimax::Random::<Rules>::default().choose_move(&self.board).unwrap();
                    if let Turn::Place(_, bug) = turn {
                        if matches!(
                            bug,
                            Bug::Beetle | Bug::Grasshopper | Bug::Ladybug | Bug::Pillbug
                        ) {
                            return turn;
                        }
                    }
                }
            } else if self.board.turn_num < 4 {
                loop {
                    let turn =
                        minimax::Random::<Rules>::default().choose_move(&self.board).unwrap();
                    if let Turn::Place(_, bug) = turn {
                        if bug == Bug::Queen {
                            return turn;
                        }
                    }
                }
            }
        }
        self.strategy.choose_move(&self.board).unwrap()
    }

    fn principal_variation(&self) -> Vec<Turn> {
        self.strategy.principal_variation()
    }

    fn set_max_depth(&mut self, depth: u8) {
        self.strategy.set_max_depth(depth);
    }

    fn set_timeout(&mut self, time: Duration) {
        self.strategy.set_timeout(time);
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn exit(msg: String) -> ! {
    eprintln!("{}", msg);
    std::process::exit(1)
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) enum PlayerStrategy {
    Iterative(ParallelOptions),
    Random,
    Mcts(MCTSOptions),
}

pub struct PlayerConfig {
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) num_threads: Option<usize>,
    pub(crate) opts: IterativeOptions,
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) strategy: PlayerStrategy,
    pub(crate) eval: BasicEvaluator,
    pub(crate) random_opening: bool,
}

#[cfg(not(target_arch = "wasm32"))]
pub fn configure_player() -> Result<(PlayerConfig, Vec<String>), pico_args::Error> {
    let mut args = pico_args::Arguments::from_env();

    let mut config = PlayerConfig::new();

    // Configure common minimax options.
    if args.contains(["-v", "--verbose"]) {
        config.opts = config.opts.verbose();
    }
    let table_size: Option<usize> = args.opt_value_from_str("--table_mb")?;
    if let Some(table_size) = table_size {
        config.opts.table_byte_size = table_size.checked_shl(20).unwrap();
    }
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
    if args.contains("--quiet-search") {
        config.opts = config.opts.with_quiescence_search_depth(2);
    }

    // 0 for num_cpu threads; >0 for specific count.
    config.num_threads = args.opt_value_from_str("--num-threads")?.map(|thread_arg: String| {
        if thread_arg == "max" || thread_arg == "all" {
            0
        } else if let Ok(num) = thread_arg.parse::<usize>() {
            num
        } else {
            exit(format!("Could not parse num_threads={}. Expected int or 'max'", thread_arg));
        }
    });

    // Configure specific strategy.
    let strategy: Option<String> = args.opt_value_from_str("--strategy")?;
    config.strategy = match strategy.as_deref().unwrap_or("iterative") {
        "random" => PlayerStrategy::Random,
        "mcts" => {
            let mut options = MCTSOptions::default()
                .with_max_rollout_depth(200)
                .with_rollouts_before_expanding(5);
            options.verbose = config.opts.verbose;
            PlayerStrategy::Mcts(options)
        }
        "mtdf" => {
            config.opts = config.opts.with_mtdf();
            config.num_threads = Some(1);
            PlayerStrategy::Iterative(ParallelOptions::new())
        }
        "iterative" => {
            let mut parallel_opts = ParallelOptions::new();
            if args.contains("--background-ponder") {
                parallel_opts = parallel_opts.with_background_pondering();
            }
            PlayerStrategy::Iterative(parallel_opts)
        }
        _ => exit(format!("Unrecognized strategy: {}", strategy.unwrap_or_default())),
    };
    Ok((config, args.finish().into_iter().map(|s| s.into_string().unwrap()).collect::<Vec<_>>()))
}

impl Default for PlayerConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl PlayerConfig {
    pub fn new() -> Self {
        Self {
            #[cfg(not(target_arch = "wasm32"))]
            num_threads: None,
            opts: IterativeOptions::new()
                .with_countermoves()
                .with_countermove_history()
                .with_table_byte_size(100 << 20),
            #[cfg(not(target_arch = "wasm32"))]
            strategy: PlayerStrategy::Iterative(ParallelOptions::new()),
            eval: BasicEvaluator::default(),
            random_opening: false,
        }
    }

    #[cfg(target_arch = "wasm32")]
    pub(crate) fn new_player(&self) -> Box<dyn Player + Send> {
        Box::new(NokamutePlayer::new(
            Box::new(IterativeSearch::new(self.eval, self.opts)),
            self.random_opening,
        ))
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) fn new_player(&self) -> Box<dyn Player + Send> {
        Box::new(match &self.strategy {
            PlayerStrategy::Random => NokamutePlayer::new_with_name(
                "random",
                Box::<Random<Rules>>::default(),
                self.random_opening,
            ),
            PlayerStrategy::Mcts(opts) => {
                let mut opts = opts.clone();
                let num_threads = self.num_threads.unwrap_or(0);
                if num_threads > 0 {
                    opts = opts.with_num_threads(num_threads);
                }
                NokamutePlayer::new(
                    Box::new(MonteCarloTreeSearch::new_with_policy(
                        opts,
                        Box::new(BiasedRollouts {}),
                    )),
                    self.random_opening,
                )
            }
            PlayerStrategy::Iterative(parallel_opts) => {
                let mut parallel_opts = *parallel_opts;
                let num_threads = self.num_threads.unwrap_or(0);
                if num_threads > 0 {
                    parallel_opts = parallel_opts.with_num_threads(num_threads);
                }
                NokamutePlayer::new(
                    if num_threads == 1 {
                        Box::new(IterativeSearch::new(self.eval, self.opts))
                    } else {
                        Box::new(ParallelSearch::new(self.eval, self.opts, parallel_opts))
                    },
                    self.random_opening,
                )
            }
        })
    }
}
