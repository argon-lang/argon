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
npm ci --prefix backend/js/api
npm run build --prefix backend/js/api
npm ci --prefix backend/js/backend
npm run build --prefix backend/js/backend
npm ci --prefix backend/js/util/copy-deploy
npm run build --prefix backend/js/util/copy-deploy
backend/jvm/gradlew -p backend/jvm :backend:installDist



# Copy
rm -rf dist
mkdir dist/
mkdir dist/arch/
mkdir dist/arch/x86_64-unknown-linux-gnu/
mkdir dist/arch/i686-unknown-linux-gnu/
mkdir dist/arch/aarch64-unknown-linux-gnu/
mkdir dist/backend/
mkdir dist/backend/jvm/
mkdir dist/bin/
cp -p target/x86_64-unknown-linux-gnu/release/argonc dist/arch/x86_64-unknown-linux-gnu/
cp -p target/i686-unknown-linux-gnu/release/argonc dist/arch/i686-unknown-linux-gnu/
cp -p target/aarch64-unknown-linux-gnu/release/argonc dist/arch/aarch64-unknown-linux-gnu/
node backend/js/util/copy-deploy/lib/main.js backend/js/backend dist/backend/js
cp -p backend/jvm/backend/build/install/backend/lib/*.jar dist/backend/jvm/

# Launchers
cp scripts/argonc dist/bin/argonc
chmod +x scripts/argonc dist/bin/argonc
