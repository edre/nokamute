#![no_main]

use libfuzzer_sys::fuzz_target;
use nokamute::{PlayerConfig, UhpServer};
use std::io::Cursor;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let config = PlayerConfig::new();
        //config.opts = config.opts.with_table_byte_size(8 << 10);
        let mut server = UhpServer::new(config, Cursor::new(Vec::new()));
        for line in s.lines() {
            let _ = server.command(line);
        }
    }
});
