# Nokamute #

Nokamute is a hive AI focused on speed.

## Features ##

The single executable can:

* Run a [Universal Hive
Protocol](https://github.com/jonthysell/Mzinga/wiki/UniversalHiveProtocol)
engine (`nokamute uhp`)
* Debug and play against a human on the command line (`nokamute cli`)
* Play against another UHP engine (`nokamute play path/to/myengine ai`)
* Run a UHP testsuite against an engine (`nokamute uhp-debug path/to/myengine`)

For a graphical interface to play against nokamute, you can use [MzingaViewer](https://github.com/jonthysell/Mzinga/wiki/MzingaViewer) and under Viewer Options, set the Engine to your nokamute executable.

## Build ##

You can get a pre-built download from the Releases page for Linux, Windows, and wasm32.

Otherwise, get a stable rust toolchain from [rustup.rs](https://rustup.rs) or any package
manager.  Run `cargo build --release` to build nokamute and its dependencies.

## Design ##

The original motivation for this project was to explore the space of boardless state representations to find an efficient one. After several iterations it has much faster move generation than any other hive AI, mostly due to:

* Using a compiled language (rust), and avoiding allocations and complex types like hashmaps in the inner loop.
* A game state representation with a 32x32 flat array of bytes that wraps across 3 axes. Each byte has presense, color, height, bug, bug number (just for generating notation). Stacked bugs are stored in a small cache off of the main grid.
* Linear [algorithm](https://en.wikipedia.org/wiki/Biconnected_component#Pseudocode) to find all pinned bugs.

The engine was developed in tandem with the generic rust [`minimax`](https://crates.io/crates/minimax) library. It implements alpha-beta and a handful of classic 20th century search optimizations. Its multithreaded implementation can make efficient use of many cores.

The evaluation function is simplistic, uses a lot of arbitary constants, and is an area in need of more development.

## Name ##

From toki pona: "many legs" or ambiguously "many branches".
