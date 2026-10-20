#[cfg(not(target_arch = "wasm32"))]
use crate::PlayerStrategy;
use crate::{BasicEvaluator, PlayerConfig, Rules};
use hexbugs::*;
use minimax::Strategy;

pub fn nokamute_version() -> &'static str {
    let version =
        git_version::git_version!(args = ["--tags", "--dirty=+"], cargo_prefix = "cargo-");
    version.strip_prefix('v').unwrap_or(version)
}

pub struct NokamuteEngine {
    config: PlayerConfig,
    strategy: Box<dyn Strategy<Rules> + Send>,
}

impl NokamuteEngine {
    pub fn new(config: PlayerConfig) -> Self {
        let strategy = config.new_strategy();
        Self { config, strategy }
    }

    pub fn reset_strategy(&mut self) {
        self.strategy = self.config.new_strategy();
    }

    fn get_option_int<Option: UhpOptionInt>(&self) -> Result<String> {
        Ok(format!(
            "{};int;{};{};{};{}",
            Option::name(),
            Option::current(&self.config)?,
            Option::current(&PlayerConfig::default())?,
            Option::min(),
            Option::max()
        ))
    }

    fn get_option_bool<Option: UhpOptionBool>(&self) -> Result<String> {
        fn fmt_bool(b: bool) -> &'static str {
            if b { "True" } else { "False" }
        }
        Ok(format!(
            "{};bool;{};{}",
            Option::name(),
            fmt_bool(Option::current(&self.config)?),
            fmt_bool(Option::current(&PlayerConfig::default())?)
        ))
    }

    fn set_option_int<Option: UhpOptionInt>(&mut self, arg: &str) -> Result<String> {
        let value = arg.parse::<usize>().map_err(|_| UhpError::InvalidOption(arg.into()))?;
        if value < Option::min() || value > Option::max() {
            return Err(UhpError::InvalidOption(arg.into()));
        }
        Option::set(value, &mut self.config);
        self.get_option_int::<Option>()
    }

    fn set_option_bool<Option: UhpOptionBool>(&mut self, arg: &str) -> Result<String> {
        let value = match arg {
            "True" => true,
            "False" => false,
            _ => return Err(UhpError::InvalidOption(arg.into())),
        };
        Option::set(value, &mut self.config);
        self.get_option_bool::<Option>()
    }
}

impl UhpEngine for NokamuteEngine {
    fn name(&self) -> &str {
        "nokamute"
    }

    fn version(&self) -> &str {
        nokamute_version()
    }

    fn best_move(&mut self, game: &GameState, limit: BestMoveLimit) -> Turn {
        match limit {
            BestMoveLimit::Depth(depth) => self.strategy.set_max_depth(depth),
            BestMoveLimit::Time(dur) => self.strategy.set_timeout(dur),
            BestMoveLimit::DepthOrTime(depth, dur) => {
                self.strategy.set_depth_or_timeout(depth, dur)
            }
        }
        if self.config.random_opening {
            if game.turn_num < 2 {
                loop {
                    let turn = minimax::Random::<Rules>::default().choose_move(game).unwrap();
                    if let Turn::Place(_, bug) = turn
                        && matches!(
                            bug,
                            Bug::Beetle | Bug::Grasshopper | Bug::Ladybug | Bug::Pillbug
                        )
                    {
                        return turn;
                    }
                }
            } else if game.turn_num < 4 {
                loop {
                    let turn = minimax::Random::<Rules>::default().choose_move(game).unwrap();
                    if let Turn::Place(_, bug) = turn
                        && bug == Bug::Queen
                    {
                        return turn;
                    }
                }
            }
        }
        self.strategy.choose_move(game).unwrap()
    }

    fn principal_variation(&self) -> Vec<Turn> {
        self.strategy.principal_variation()
    }

    fn get_options(&self) -> Vec<String> {
        vec![
            self.get_option_int::<AggressionOption>().unwrap(),
            #[cfg(not(target_arch = "wasm32"))]
            self.get_option_bool::<BackgroundPonderingOption>().unwrap(),
            #[cfg(not(target_arch = "wasm32"))]
            self.get_option_int::<NumThreadsOption>().unwrap(),
            self.get_option_bool::<RandomOpeningOption>().unwrap(),
            self.get_option_int::<TableSizeOption>().unwrap(),
            self.get_option_bool::<VerboseOption>().unwrap(),
        ]
    }

    fn set_option(&mut self, name: &str, value: &str) -> Result<String> {
        let updated = match name {
            "Aggression" => self.set_option_int::<AggressionOption>(value)?,
            #[cfg(not(target_arch = "wasm32"))]
            "BackgroundPondering" => self.set_option_bool::<BackgroundPonderingOption>(value)?,
            #[cfg(not(target_arch = "wasm32"))]
            "NumThreads" => self.set_option_int::<NumThreadsOption>(value)?,
            "RandomOpening" => self.set_option_bool::<RandomOpeningOption>(value)?,
            "TableSizeMiB" => self.set_option_int::<TableSizeOption>(value)?,
            "Verbose" => self.set_option_bool::<VerboseOption>(value)?,
            _ => return Err(UhpError::InvalidOption(name.into())),
        };
        self.reset_strategy();
        Ok(updated)
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn uhp_serve(config: PlayerConfig) {
    hexbugs::uhp_serve(NokamuteEngine::new(config));
}

trait UhpOptionInt {
    fn name() -> &'static str;
    fn current(config: &PlayerConfig) -> Result<usize>;
    fn min() -> usize;
    fn max() -> usize;
    // Caller does bounds checking.
    fn set(value: usize, config: &mut PlayerConfig);
}

#[cfg(not(target_arch = "wasm32"))]
struct NumThreadsOption {}
#[cfg(not(target_arch = "wasm32"))]
impl UhpOptionInt for NumThreadsOption {
    fn name() -> &'static str {
        "NumThreads"
    }
    fn current(config: &PlayerConfig) -> Result<usize> {
        Ok(if let Some(num_threads) = config.num_threads { num_threads } else { Self::max() })
    }
    fn min() -> usize {
        1
    }
    fn max() -> usize {
        minimax::ParallelOptions::default().num_threads()
    }
    fn set(value: usize, config: &mut PlayerConfig) {
        config.num_threads = Some(value)
    }
}

struct AggressionOption {}
impl UhpOptionInt for AggressionOption {
    fn name() -> &'static str {
        "Aggression"
    }
    fn current(config: &PlayerConfig) -> Result<usize> {
        Ok(config.eval.aggression().into())
    }
    fn min() -> usize {
        1
    }
    fn max() -> usize {
        5
    }
    fn set(value: usize, config: &mut PlayerConfig) {
        config.eval = BasicEvaluator::new(value as u8);
    }
}

struct TableSizeOption {}
impl UhpOptionInt for TableSizeOption {
    fn name() -> &'static str {
        "TableSizeMiB"
    }
    fn current(config: &PlayerConfig) -> Result<usize> {
        Ok(config.opts.table_byte_size >> 20)
    }
    fn min() -> usize {
        1
    }
    fn max() -> usize {
        256
    }
    fn set(value: usize, config: &mut PlayerConfig) {
        config.opts.table_byte_size = value << 20
    }
}

trait UhpOptionBool {
    fn name() -> &'static str;
    fn current(config: &PlayerConfig) -> Result<bool>;
    fn set(value: bool, config: &mut PlayerConfig);
}

struct VerboseOption {}
impl UhpOptionBool for VerboseOption {
    fn name() -> &'static str {
        "Verbose"
    }
    fn current(config: &PlayerConfig) -> Result<bool> {
        Ok(config.opts.verbose)
    }
    fn set(value: bool, config: &mut PlayerConfig) {
        config.opts.verbose = value;
    }
}

struct RandomOpeningOption {}
impl UhpOptionBool for RandomOpeningOption {
    fn name() -> &'static str {
        "RandomOpening"
    }
    fn current(config: &PlayerConfig) -> Result<bool> {
        Ok(config.random_opening)
    }
    fn set(value: bool, config: &mut PlayerConfig) {
        config.random_opening = value;
    }
}

#[cfg(not(target_arch = "wasm32"))]
struct BackgroundPonderingOption {}
#[cfg(not(target_arch = "wasm32"))]
impl UhpOptionBool for BackgroundPonderingOption {
    fn name() -> &'static str {
        "BackgroundPondering"
    }
    fn current(config: &PlayerConfig) -> Result<bool> {
        let parallel_opts = if let PlayerStrategy::Iterative(parallel_opts) = config.strategy {
            parallel_opts
        } else {
            return Err(UhpError::EngineError("Unexpected config".into()));
        };
        Ok(parallel_opts.background_pondering)
    }
    fn set(value: bool, config: &mut PlayerConfig) {
        if let PlayerStrategy::Iterative(ref mut parallel_opts) = config.strategy {
            parallel_opts.background_pondering = value;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    fn test_server() -> UhpServer<NokamuteEngine, Cursor<Vec<u8>>> {
        let mut config = PlayerConfig::new();
        config.opts = config.opts.with_table_byte_size(1 << 20);
        #[cfg(not(target_arch = "wasm32"))]
        {
            config.num_threads = Some(1);
        }
        UhpServer::new(NokamuteEngine::new(config), Cursor::new(Vec::new()))
    }

    fn command(server: &mut UhpServer<NokamuteEngine, Cursor<Vec<u8>>>, input: &str) -> String {
        server.swap_output(Cursor::new(Vec::new()));
        server.command(input);
        let buf = server.swap_output(Cursor::new(Vec::new()));
        String::from_utf8(buf.into_inner()).unwrap().trim().to_string()
    }

    #[test]
    fn bestmove_on_finished_game_returns_error_instead_of_panicking() {
        let mut server = test_server();
        // Same six-move-repetition draw position exercised by
        // game::tests::test_winner_sixmove_cycle: Rules::get_winner() is Some(Draw) here.
        command(
            &mut server,
            r"newgame Base+ML;White[10];wB1;bB1 wB1-;wQ /wB1;bQ bB1\;wA1 -wB1;bQ /bB1;wA1 -wQ;bB1 bQ;wA1 /wQ;bB1 wB1;wA1 -bB1;bB1 bQ/;wA1 -wQ;bB1 bQ;wA1 /wQ;bB1 wB1;wA1 -bB1;bB1 bQ/",
        );

        assert!(command(&mut server, "bestmove depth 1").contains("err"));
    }
}
