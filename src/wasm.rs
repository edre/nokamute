use crate::{PlayerConfig, UhpServer};
use std::io::Cursor;
use std::sync::{LazyLock, Mutex};
use wasm_bindgen::prelude::*;

static UHP_SERVER: LazyLock<Mutex<UhpServer<Cursor<Vec<u8>>>>> = LazyLock::new(|| {
    let mut config = PlayerConfig::new();
    config.opts = config.opts.with_table_byte_size(8 << 20);
    Mutex::new(UhpServer::new(config, Cursor::new(Vec::new())))
});

#[wasm_bindgen]
pub fn uhp(args: &str) -> String {
    let mut server = UHP_SERVER.lock().unwrap();
    server.swap_output(Cursor::new(Vec::new()));
    server.command(args);
    let buf = server.swap_output(Cursor::new(Vec::new()));
    String::from_utf8(buf.into_inner())
        .unwrap_or_else(|_| "err encoding".to_string())
        .trim()
        .to_string()
}

#[cfg(test)]
pub mod test {
    use super::uhp;
    use wasm_bindgen_test::*;
    wasm_bindgen_test_configure!(run_in_browser);

    #[wasm_bindgen_test]
    fn info_test() {
        let info = uhp("info");
        assert!(info.contains("nokamute"));
    }

    #[wasm_bindgen_test]
    fn valid_moves_test() {
        uhp("newgame Base");
        let out = uhp("validmoves");
        let mut moves = out.split(";").collect::<Vec<&str>>();
        moves.sort();
        assert_eq!(moves, &["wA1", "wB1", "wG1", "wS1"]);
    }

    #[wasm_bindgen_test]
    fn play_test() {
        uhp("newgame Base");
        uhp("play wA1");
        uhp("play bB1 -wA1");
        let state = uhp("play wQ wA1-");
        assert_eq!(state, "Base;InProgress;Black[2];wA1;bB1 -wA1;wQ wA1-");
    }

    #[wasm_bindgen_test]
    fn bestmove_depth_test() {
        uhp("newgame Base");
        let best = uhp("bestmove depth 1");
        assert!(["wA1", "wB1", "wG1", "wS1"].contains(&best.as_str()));
    }

    #[wasm_bindgen_test]
    fn bestmove_time_test() {
        uhp("newgame Base");
        let best = uhp("bestmove time 00:00:01");
        assert!(["wA1", "wB1", "wG1", "wS1"].contains(&best.as_str()));
    }

    #[wasm_bindgen_test]
    fn options_test() {
        assert!(uhp("options").contains("TableSizeMiB"));
        assert_eq!(uhp("options get TableSizeMiB"), "TableSizeMiB;int;8;100;1;256");
        assert!(uhp("options fdjskl").contains("err"));
        assert!(uhp("options get fakeoption").contains("err"));
        assert!(uhp("options set fakeoption").contains("err"));
        assert!(uhp("options set TableSizeMiB True").contains("err"));
        assert!(uhp("options set TableSizeMiB 0").contains("err"));
        assert!(uhp("options set TableSizeMiB 500").contains("err"));
        assert_eq!(uhp("options set TableSizeMiB 2"), "TableSizeMiB;int;2;100;1;256");
        assert_eq!(uhp("options get TableSizeMiB"), "TableSizeMiB;int;2;100;1;256");
    }
}
