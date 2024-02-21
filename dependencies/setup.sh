#!/bin/bash -e

EMSDK_TAG=3.1.51
EMSCRIPTEN_VERSION=3.1.51
EMSDK_NEEDS_INSTALL=false

if [ ! -d emsdk ]; then
    git clone https://github.com/emscripten-core/emsdk.git
    EMSDK_NEEDS_INSTALL=true
fi


pushd emsdk > /dev/null
if [ "$(git rev-parse HEAD)" != "$(git rev-list -n 1 "$EMSDK_TAG")" ]; then
    git fetch --tags origin
    git checkout --detach "$EMSDK_TAG"
    EMSDK_NEEDS_INSTALL=true
fi
if [ "$EMSDK_NEEDS_INSTALL" = true ]; then
    ./emsdk install "$EMSCRIPTEN_VERSION"
    ./emsdk activate "$EMSCRIPTEN_VERSION"
fi
EMSDK_QUIET=1 . ./emsdk_env.sh
popd > /dev/null


LUA_VERSION=5.4.6

mkdir -p lua
if [ ! -d lua/lua-"$LUA_VERSION" ]; then
    pushd lua > /dev/null
    curl -L -R -O "https://www.lua.org/ftp/lua-$LUA_VERSION.tar.gz"
    sha256sum --quiet -c ../lua-SHA256SUM.txt
    tar xf "lua-$LUA_VERSION.tar.gz"
    rm "lua-$LUA_VERSION.tar.gz"
    popd > /dev/null
fi



if [ ! -f lua/lua-5.4.6.wasm ]; then
    pushd lua/lua-5.4.6 > /dev/null
    make all -j"$(nproc --all)"
    popd > /dev/null
fi
