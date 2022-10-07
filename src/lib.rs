#[macro_use]
extern crate lazy_static;

mod board;
pub use board::*;
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
mod cli;
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub use cli::*;
mod eval;
pub use eval::*;
mod perft;
pub use perft::*;
mod player;
pub use player::*;
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
mod uhp_client;
mod uhp_server;
pub use uhp_server::*;
mod uhp_util;
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod wasm;
