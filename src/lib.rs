#[macro_use]
extern crate lazy_static;

mod board;
pub use board::*;
mod cli;
pub use cli::*;
mod eval;
pub use eval::*;
mod perft;
pub use perft::*;
mod player;
pub use player::*;
mod uhp_client;
mod uhp_server;
pub use uhp_server::UhpServer;
mod uhp_util;
