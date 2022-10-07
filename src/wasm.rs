use crate::{PlayerConfig, UhpServer};
use std::io::Cursor;
use wasm_bindgen::prelude::*;

static mut UHP_SERVER: *mut UhpServer<Cursor<Vec<u8>>> = std::ptr::null_mut();

#[wasm_bindgen]
pub fn uhp(args: &str) -> String {
    // Manual lazy_static.
    let server = unsafe {
        if UHP_SERVER.is_null() {
            let mut config = PlayerConfig::new();
            config.opts = config.opts.with_table_byte_size(8 << 20);
            UHP_SERVER = Box::into_raw(Box::new(UhpServer::new(config, Cursor::new(Vec::new()))));
        }
        UHP_SERVER.as_mut().unwrap()
    };
    server.swap_output(Cursor::new(Vec::new()));
    server.command(args);
    let buf = server.swap_output(Cursor::new(Vec::new()));
    String::from_utf8(buf.into_inner()).unwrap_or_else(|_| "err encoding".to_string())
}
