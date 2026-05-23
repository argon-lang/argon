#!/bin/sh -e

# Build
cargo zigbuild --release --target x86_64-unknown-linux-gnu.2.17
cargo zigbuild --release --target i686-unknown-linux-gnu.2.17
cargo zigbuild --release --target aarch64-unknown-linux-gnu.2.17
cargo build --release --target x86_64-pc-windows-gnu
cargo build --release --target i686-pc-windows-gnu

# Test
