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

async function copyInputFile(inputFile, destination) {
    const input = await inputFile.open();
    const output = await fs.open(destination, "w");
    const buffer = new Uint8Array(64 * 1024);
    try {
        while (true) {
            const count = await input.read(buffer);
            if (count === 0) break;
            await output.write(buffer.subarray(0, count));
        }
    } finally {
        await input.close();
        await output.close();
    }
}

async function copyOutputFile(source, outputFile) {
    const input = await fs.open(source, "r");
    const output = await outputFile.open();
    const buffer = new Uint8Array(64 * 1024);
    try {
        while (true) {
            const { bytesRead } = await input.read(buffer);
            if (bytesRead === 0) break;
            await output.write(buffer.subarray(0, bytesRead));
        }
    } finally {
        await input.close();
        await output.close();
    }
}

async function runJava(args) {
    const { spawn } = await import("node:child_process");
    return await new Promise((resolve, reject) => {
        const child = spawn("java", args, { stdio: ["ignore", "pipe", "pipe"] });
        const chunks = [];
        child.stdout.on("data", chunk => chunks.push(chunk));
        child.stderr.on("data", chunk => chunks.push(chunk));
        child.on("error", reject);
        child.on("close", (code, signal) => resolve({ code, signal, output: Buffer.concat(chunks).toString() }));
    });
}

globalThis.__argon_run_jvm_backend = async (task, options) => {
    const os = await import("node:os");
    const temp = await fs.mkdtemp(path.join(os.tmpdir(), "argon-jvm-"));
    try {
        const outputPath = path.join(temp, task === "codegen" ? "output.jar" : "platform-metadata.esx");
        const args = ["--module-path", path.resolve(wasmDirectory, "../../backend/jvm"), "--module", "dev.argon.backend"];
        if (task === "platform-metadata") {
            args.push("platform-metadata", "jvm");
            for (let i = 0; i < options.externFiles.length; ++i) {
                const input = options.externFiles[i];
                const inputPath = path.join(temp, `extern-${i}-${path.basename(input.fileName || "input.class")}`);
                await copyInputFile(input, inputPath);
                args.push("--extern", inputPath);
            }
            args.push("--output-file", outputPath);
        } else if (task === "codegen") {
            const inputPath = path.join(temp, `input-${path.basename(options.inputFile.fileName || "input.avm")}`);
            await copyInputFile(options.inputFile, inputPath);
            args.push("codegen", "jvm", "--input", inputPath, "--output", outputPath);
            if (options.executable) args.push("--executable");
        } else {
            throw new Error(`unknown JVM backend task: ${task}`);
        }

        const result = await runJava(args);
        if (result.code !== 0) {
            throw new Error(`java exited with status ${result.code ?? `signal ${result.signal}`}\n${result.output}`);
        }
        await copyOutputFile(outputPath, options.outputFile);
    } catch (error) {
        await options.outputFile.delete();
        throw error;
    } finally {
        await fs.rm(temp, { recursive: true, force: true });
    }
};

const argonc = await import(pathToFileURL(path.join(wasmDirectory, "argon_wasm.js")));

try {
    process.exitCode = await argonc.main(process.argv.slice(1), {
        write: (s) => process.stdout.write(s),
    });
} finally {
    delete globalThis.__argon_wasm_imports;
    delete globalThis.__argon_run_jvm_backend;
}
