sudo apt install mingw-w64

export CARGO_PROFILE_RELEASE_LTO=true
export CARGO_PROFILE_RELEASE_CODEGEN_UNITS=1

cargo build --release
mv target/release/nokamute .
strip nokamute
tar czf nokamute_linux_amd64.tar.gz --owner=0 --group=0 nokamute

cargo build --release --target x86_64-pc-windows-gnu
mv target/x86_64-pc-windows-gnu/release/nokamute.exe .
zip nokamute_win64.zip nokamute.exe
