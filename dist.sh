#!/bin/sh -e

cd "$(readlink -f "$(dirname "$0")")"
exec cargo run --manifest-path argon-dist/Cargo.toml -- "$@"
