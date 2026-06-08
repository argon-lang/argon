#!/bin/sh -e

cd "$(readlink -f "$(dirname "$0")")"

# Build a dist directory
# Layout:
# dist/arch/<arch>/bin - directory with platform specific binaries (including wasm)
# dist/backend/<backend> - directory for the backends
# dist/bin - directory to add to PATH, shell scripts/launchers/etc


# Build
cargo zigbuild --bin argonc --release --target x86_64-unknown-linux-gnu.2.17
cargo zigbuild --bin argonc --release --target i686-unknown-linux-gnu.2.17
cargo zigbuild --bin argonc --release --target aarch64-unknown-linux-gnu.2.17
cargo zigbuild --bin argonc --release --target x86_64-pc-windows-gnullvm
cargo zigbuild --bin argonc --release --target i686-pc-windows-gnullvm
cargo zigbuild --bin argonc --release --target aarch64-pc-windows-gnullvm
cargo zigbuild --bin argonc_launcher_windows --profile small --target aarch64-pc-windows-gnullvm
npm ci --prefix backend/api/js
npm run build --prefix backend/api/js
npm ci --prefix backend/backends/js
npm run build --prefix backend/backends/js
npm ci --prefix backend/util/js-copy-deploy
npm run build --prefix backend/util/js-copy-deploy



# Copy
rm -rf dist
mkdir dist/
mkdir dist/arch/
mkdir dist/arch/x86_64-unknown-linux-gnu/
mkdir dist/arch/i686-unknown-linux-gnu/
mkdir dist/arch/aarch64-unknown-linux-gnu/
mkdir dist/arch/x86_64-pc-windows-gnullvm/
mkdir dist/arch/i686-pc-windows-gnullvm/
mkdir dist/backend/
mkdir dist/bin/
cp -p target/x86_64-unknown-linux-gnu/release/argonc dist/arch/x86_64-unknown-linux-gnu/
cp -p target/i686-unknown-linux-gnu/release/argonc dist/arch/i686-unknown-linux-gnu/
cp -p target/aarch64-unknown-linux-gnu/release/argonc dist/arch/aarch64-unknown-linux-gnu/
cp -p target/x86_64-pc-windows-gnullvm/release/argonc.exe dist/arch/x86_64-pc-windows-gnullvm/
cp -p target/i686-pc-windows-gnullvm/release/argonc.exe dist/arch/i686-pc-windows-gnullvm/
node backend/util/js-copy-deploy/lib/main.js backend/backends/js dist/backend/js

# Launchers
cp scripts/argonc dist/bin/argonc
chmod +x scripts/argonc dist/bin/argonc
cp -p target/i686-pc-windows-gnullvm/small/argonc_launcher_windows.exe dist/bin/argonc.exe
