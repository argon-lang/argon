#!/usr/bin/env node

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { pathToFileURL } from "node:url";

const wasmDirectory = import.meta.dirname;
const z3Bytes = await fs.readFile(path.join(wasmDirectory, "z3.wasm"));
const textDecoder = new TextDecoder();
let z3Memory;

const minimalWasi = {
    proc_exit(code) {
        throw new Error(`WASI exit with code ${code}`);
    },

    clock_time_get(_clockid, _precision, timePtr) {
        if (!z3Memory) {
            return 0;
        }

        const nowNs = BigInt(Date.now()) * 1_000_000n;
        new DataView(z3Memory.buffer).setBigUint64(timePtr, nowNs, true);
        return 0;
    },

    fd_close(_fd) {
        return 0;
    },

    fd_write(fd, iovsPtr, iovsLen, nwrittenPtr) {
        if (!z3Memory) {
            return 0;
        }

        const view = new DataView(z3Memory.buffer);
        let written = 0;

        for (let i = 0; i < iovsLen; i++) {
            const ptr = view.getUint32(iovsPtr + i * 8, true);
            const len = view.getUint32(iovsPtr + i * 8 + 4, true);
            const bytes = new Uint8Array(z3Memory.buffer, ptr, len);
            const text = textDecoder.decode(bytes);

            if (fd === 1) {
                console.log(text);
            } else if (fd === 2) {
                console.error(text);
            }

            written += len;
        }

        view.setUint32(nwrittenPtr, written, true);
        return 0;
    },

    fd_read(_fd, _iovsPtr, _iovsLen, nreadPtr) {
        if (!z3Memory) {
            return 0;
        }

        new DataView(z3Memory.buffer).setUint32(nreadPtr, 0, true);
        return 0;
    },

    fd_seek(_fd, _offset, _whence, newOffsetPtr) {
        if (!z3Memory) {
            return 0;
        }

        new DataView(z3Memory.buffer).setBigUint64(newOffsetPtr, 0n, true);
        return 0;
    },

    environ_sizes_get(countPtr, bufSizePtr) {
        if (!z3Memory) {
            return 0;
        }

        const view = new DataView(z3Memory.buffer);
        view.setUint32(countPtr, 0, true);
        view.setUint32(bufSizePtr, 0, true);
        return 0;
    },

    environ_get(_environPtr, _environBufPtr) {
        return 0;
    },
};

const { instance: z3 } = await WebAssembly.instantiate(z3Bytes, {
    wasi_snapshot_preview1: minimalWasi,
});
z3Memory = z3.exports.memory;

const memoryBytes = await fs.readFile(path.join(wasmDirectory, "argon-memory.wasm"));
const { instance: memory } = await WebAssembly.instantiate(memoryBytes, {
    z3: z3.exports,
});

globalThis.__argon_wasm_imports = {
    "argon-memory": memory.exports,
    z3: z3.exports,
};

const backendPath = path.resolve(wasmDirectory, "../../backend/js");

globalThis.__argon_run_backend = async (task, options) => {
    const backendUrl = pathToFileURL(path.join(backendPath, "lib/index.js"));
    const apiUrl = pathToFileURL(path.join(backendPath, "node_modules/@argon-lang/js-backend-api/lib/metadata.js"));
    const binaryUrl = pathToFileURL(path.join(backendPath, "node_modules/@argon-lang/esexpr/lib/binary_format.js"));
    const backend = await import(backendUrl);
    const { PlatformMetadataResult } = await import(apiUrl);
    const { writeExprs } = await import(binaryUrl);

    if (task === "platform-metadata") {
        const metadata = await backend.loadMetadata({
            packageName: options.packageName,
            externFiles: options.externFiles,
        });
        const expr = PlatformMetadataResult.codec.encode({ ...metadata, platform: "js" });
        const stream = await options.outputFile.open();
        try {
            for await (const chunk of writeExprs([expr])) await stream.write(chunk);
        } finally {
            await stream.close();
        }
    } else {
        await backend.codegen({
            tube: options.inputFile,
            outputDirectory: options.outputDirectory,
            executable: options.executable,
        });
    }
};

const argonc = await import(pathToFileURL(path.join(wasmDirectory, "argon_wasm.js")));

try {
    process.exitCode = await argonc.main(process.argv.slice(1), {
        write: (s) => process.stdout.write(s),
    });
} finally {
    delete globalThis.__argon_wasm_imports;
}
