# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Nokamute

Nokamute is a high-performance Hive board game engine in Rust (~4.6K LOC). It supports native CLI, UHP (Universal Hive Protocol) server, Python bindings (PyO3), and WebAssembly targets.

## Build & Test Commands

```bash
# Build
cargo build
cargo build --release

# Run tests (always run both feature sets)
cargo test && cargo test --features=smaller-grid

# UHP correctness suite (runs against itself)
cargo run -- --strategy=random uhp-debug target/debug/nokamute --strategy=random

# Lint & format
cargo clippy --no-deps -- -D warnings
cargo fmt -- --check

# Benchmarks
cargo build --benches

# Python bindings (requires maturin)
maturin develop --features python

# WebAssembly
rustup target add wasm32-unknown-unknown
RUSTFLAGS='--cfg getrandom_backend="wasm_js"' wasm-pack test --firefox --headless

# Optimized release build
CARGO_PROFILE_RELEASE_LTO=true CARGO_PROFILE_RELEASE_CODEGEN_UNITS=1 cargo build --release
```

## Architecture

### Board Representation (board.rs — the core)
The board is a **32x32 flat byte array hex grid** that wraps on all 3 axes like a torus. Each cell is a single `Node` byte with bit-packed fields: color (bit 7), bug type (bits 4-6), bug number (bits 2-3), stack height (bits 0-1). This design prioritizes cache locality and minimal allocation. An "underworld cache" handles stacks above height 3.

### Move Generation
Implements the `minimax::Game` trait. Uses a **linear biconnected component algorithm** to detect pinned pieces (pieces whose removal would disconnect the hive). This is the most performance-critical code path.

### Search & Evaluation
- Integrates with the external `minimax` crate (0.6.0) for alpha-beta with iterative deepening, parallel search, transposition tables
- Strategy options: `iterative`, `mcts`, `mtdf`, `random` (via `--strategy` flag)
- `BasicEvaluator` scores based on queen liberties, movable pieces, unplayed pieces; coefficients are acknowledged as rough ("made up") and marked for improvement

### Interface Layers
- **UHP Server** (`uhp_server.rs`): stdin/stdout protocol, compatible with MzingaViewer. Supports Mosquito, Ladybug, Pillbug expansions.
- **CLI** (`cli.rs`): Interactive terminal play with Unicode board rendering.
- **Python** (`python.rs`): PyO3 bindings exposing `GameState`, `PyMove`, `PyPiece` classes. Includes `encode_board()` for ML tensor output (84x32x32 numpy array). Built with `maturin`.
- **WASM** (`wasm.rs`): Single `uhp(args) -> str` function wrapping a lazy-locked UHP server.

### Entry Point (main.rs)
CLI dispatcher with subcommands: `cli`, `uhp`, `play`, `perft`, `uhp-debug`. Key flags: `--strategy`, `--table-mb` (transposition table size), `--threads`.

## Cargo Features

- `larger-grid` (default) — 32x32 hex grid
- `smaller-grid` — 16x16 grid, more memory efficient but less accurate at edges
- `python` — enables PyO3 + numpy bindings, changes crate type to cdylib+rlib. Use `cargo check --lib --features python` to verify (not `cargo build`, which fails at link time without a Python interpreter).

## Key Design Decisions

- **Performance is the primary goal.** Flat arrays, packed structs, minimal allocations. Changes should not regress cache behavior or add unnecessary indirection.
- **Zobrist hashing** for board state (lazy-initialized via `OnceLock`).
- **Deterministic move ordering** in Python bindings (via `sort_key()`) for reproducible ML training.
- **Canonical piece type ordering** (`bug_sort_ord`): Queen=0, Beetle=1, Grasshopper=2, Ant=3, Spider=4, Mosquito=5, Ladybug=6, Pillbug=7. This differs from the `Bug` enum discriminants and is used by both tensor encoding and move sorting.
- Bug types are an enum with 8 variants: Queen, Grasshopper, Spider, Ant, Beetle, Mosquito, Ladybug, Pillbug (note: enum order differs from sort order above).
