
import { parse as parseArgs } from "ts-command-line-args";

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { codegen } from "./index.js";

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

const codegenOutput = codegen({
    tubeMapping: [],

    tubeInput: {
        type: "ir-encoded",
        data() {
            return readUint8ArrayStream(args.input);
        },
    },

    async *getExterns() {
        for(const sourceFile of args.externs) {
            const sourceCode = await fs.readFile(sourceFile, { encoding: "utf-8" });            
            yield {
                sourceCode,
                sourceFile,
            };
        }
    },
});

for await(const moduleCodegenResult of codegenOutput) {
    const fileName = path.join(args.output, ...moduleCodegenResult.moduleFilePath);

    await fs.mkdir(path.dirname(fileName), { recursive: true });
    await fs.writeFile(fileName, moduleCodegenResult.sourceCode, { encoding: "utf-8" });
}

async function* readUint8ArrayStream(path: string): AsyncIterable<Uint8Array> {
    const file = await fs.open(path);
    try {
        const buffer = new Uint8Array(16384);

        for(;;) {
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



