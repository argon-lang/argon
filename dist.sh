#!/bin/sh -e

cd "$(readlink -f "$(dirname "$0")")"

# Build a dist directory
# Layout:
# dist/arch/<arch>/bin - directory with platform specific binaries (including wasm)
# dist/backend/<backend> - directory for the backends
# dist/bin - directory to add to PATH, shell scripts/launchers/etc


# Build
cargo zigbuild --release --target x86_64-unknown-linux-gnu.2.17
cargo zigbuild --release --target i686-unknown-linux-gnu.2.17
cargo zigbuild --release --target aarch64-unknown-linux-gnu.2.17
cargo build --release --target x86_64-pc-windows-gnu
cargo build --release --target i686-pc-windows-gnu
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
mkdir dist/arch/x86_64-pc-windows-gnu/
mkdir dist/arch/i686-pc-windows-gnu/
mkdir dist/backend/
mkdir dist/bin/
cp -p target/x86_64-unknown-linux-gnu/release/argonc dist/arch/x86_64-unknown-linux-gnu/
cp -p target/i686-unknown-linux-gnu/release/argonc dist/arch/i686-unknown-linux-gnu/
cp -p target/aarch64-unknown-linux-gnu/release/argonc dist/arch/aarch64-unknown-linux-gnu/
cp -p target/x86_64-pc-windows-gnu/release/argonc.exe dist/arch/x86_64-pc-windows-gnu/
cp -p target/i686-pc-windows-gnu/release/argonc.exe dist/arch/i686-pc-windows-gnu/
node backend/util/js-copy-deploy/lib/main.js backend/backends/js dist/backend/js

# Scripts
cp scripts/argonc dist/bin/argonc
chmod +x scripts/argonc dist/bin/argonc
