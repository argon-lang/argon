
import { parse as parseArgs } from "ts-command-line-args";

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { codegen2 } from "./index.js";
import type { PromiseWithError } from "@argon-lang/noble-idl-core/util";
import type { ScopedResource, Stream } from "./backend.js";
import { TubeFileEntry } from "./vm-format.js";
import { readExprStream } from "@argon-lang/esexpr/binary_format";
import { ESExpr } from "@argon-lang/esexpr";

interface CliArgs {
    input: string,
    output: string,
    externs: string[],
    help?: boolean,
}

const args = parseArgs<CliArgs>(
    {
        input: { type: String, alias: "i" },
        output: { type: String, alias: "o" },
        externs: { type: String, alias: "e", multiple: true, defaultValue: [] },
        help: { type: Boolean, optional: true, alias: "h" },
    },
    {
        helpArg: "help",
        processExitCode: 1,
        stopAtFirstUnknown: true,
    },
);

const codegenOutput = codegen2({
    options: {
        externs: args.externs.map(sourceFile => ({
            fileName: sourceFile,
            asBytes() {
                return readUint8ArrayStream(sourceFile);
            },
        })),
    },

    tubeInput: {
        async stream(): PromiseWithError<ScopedResource<Stream<unknown, TubeFileEntry>>, unknown> {
            const iter = decodeIrEntries(readExprStream(readUint8ArrayStream(args.input)))[Symbol.asyncIterator]();

            let iterDone = false;

            const stream: Stream<unknown, TubeFileEntry> = {
                async next() {
                    if(iterDone) {
                        return [];
                    }

                    const res = await iter.next();
                    if(res.done) {
                        iterDone = true;
                        return [];
                    }
                    else {
                        return [ res.value ];
                    }
                },
            };

            return {
                async get() {
                    return stream;
                },

                async close() {
                    if(!iterDone) {
                        iterDone = true;
                        if("return" in iter) {
                            iter.return();
                        }
                    }
                }
            };
        }
    },
});

for await(const entry of codegenOutput) {
    const fileName = path.join(args.output, ...entry.dirs, entry.fileName);

    await fs.mkdir(path.dirname(fileName), { recursive: true });
    writeUint8ArrayStream(fileName, entry.resource.asBytes());
}

async function* readUint8ArrayStream(path: string): AsyncIterable<Uint8Array> {
    const file = await fs.open(path);
    try {
        for(;;) {
            const buffer = new Uint8Array(16384);
            const { bytesRead } = await file.read(buffer);
            if(bytesRead === 0) {
                break;
            }
            
            yield buffer.slice(0, bytesRead);
        }
    }
    finally {
        await file.close();
    }
}


async function* decodeIrEntries(exprs: AsyncIterable<ESExpr>): AsyncIterable<TubeFileEntry> {
    for await(const expr of exprs) {
        const res = TubeFileEntry.codec.decode(expr);
        if(!res.success) {
            throw new Error("Could not decode expression as Argon VM IR: " + res.message);
        }

        yield res.value;
    }
}

async function writeUint8ArrayStream(path: string, data: AsyncIterable<Uint8Array>): Promise<void> {
    const file = await fs.open(path, "w");
    try {
        for await(const buffer of data) {
            await file.write(buffer);
        }
    }
    finally {
        await file.close();
    }
}


