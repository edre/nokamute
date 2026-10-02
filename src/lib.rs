#[cfg(not(target_arch = "wasm32"))]
mod cli;
#[cfg(not(target_arch = "wasm32"))]
pub use cli::*;
mod eval;
pub use eval::*;
#[cfg(not(target_arch = "wasm32"))]
mod mcts;
#[cfg(not(target_arch = "wasm32"))]
mod perft;
#[cfg(not(target_arch = "wasm32"))]
pub use perft::*;
mod player;
pub use player::*;
mod rules;
pub use rules::*;
#[cfg(not(target_arch = "wasm32"))]
mod uhp_client;
mod uhp_server;
pub use uhp_server::*;
#[cfg(target_arch = "wasm32")]
mod wasm;
