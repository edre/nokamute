sudo apt install mingw-w64

echo "[profile.release]" >> Cargo.toml
echo "lto = true" >> Cargo.toml
echo "codegen-units = 1" >> Cargo.toml

cargo build --release
mv target/release/nokamute nokamute_linux_amd64
strip nokamute_linux_amd64
gzip -f nokamute_linux_amd64

cargo build --release --target x86_64-pc-windows-gnu
mv target/x86_64-pc-windows-gnu/release/nokamute.exe .
zip nokamute_win64.zip nokamute.exe

git checkout Cargo.toml
