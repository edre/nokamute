#[macro_use]
extern crate lazy_static;

mod board;
pub use board::*;
mod bug;
pub use bug::*;
#[cfg(not(target_arch = "wasm32"))]
mod cli;
#[cfg(not(target_arch = "wasm32"))]
pub use cli::*;
mod eval;
pub use eval::*;
mod hex_grid;
pub use hex_grid::*;
mod notation;
#[cfg(not(target_arch = "wasm32"))]
mod perft;
#[cfg(not(target_arch = "wasm32"))]
pub use perft::*;
mod player;
pub use player::*;
#[cfg(not(target_arch = "wasm32"))]
mod uhp_client;
mod uhp_server;
pub use uhp_server::*;
#[cfg(not(target_arch = "wasm32"))]
mod wasm;
