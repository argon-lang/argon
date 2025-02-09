import { TubeFileEntry } from "./vm-format.js";

import { readExprStream } from "@argon-lang/esexpr/binary_format";

import { parse as parseArgs } from "ts-command-line-args";
import * as astring from "astring";
import * as acorn from "acorn";
import type * as estree from "estree";

import * as fs from "node:fs/promises";
import * as path from "node:path";
import { ExternLoader } from "./externs.js";
import { getModuleOutputFile } from "./util.node.js";
import { TubeEmitter, type EmitOptions } from "./emitter.js";
import { readIR } from "./ir-reader.js";

interface CliArgs {
    input: string,
    output: string,
    externs: string[],
}

const args = parseArgs<CliArgs>({
    input: { type: String, alias: "i" },
    output: { type: String, alias: "o" },
    externs: { type: String, alias: "e", multiple: true, defaultValue: [] },
});

const externLoader = new ExternLoader();

for(const externPath of args.externs) {
    const externCode = await fs.readFile(externPath, { encoding: "utf-8" });
    const program: acorn.Program = acorn.parse(externCode, {
        ecmaVersion: 2024,
        sourceType: "module",
        sourceFile: externPath,
        allowHashBang: false,
    });
    externLoader.addExterns(program as estree.Program);
}


const programModel = await readIR(loadIrEntries(args.input));

const options: EmitOptions = {
    program: programModel,
    externProvider: externLoader,
    tubeMapping: [],
};

const tubeEmitter = new TubeEmitter(options);

for(const emittedModule of tubeEmitter.emit()) {
    const fileName = getModuleOutputFile(args.output, emittedModule.modulePath);
    const text = astring.generate(emittedModule.jsProgram);

    await fs.mkdir(path.dirname(fileName), { recursive: true });
    await fs.writeFile(fileName, text, { encoding: "utf-8" });
}


async function* loadIrEntries(path: string): AsyncIterable<TubeFileEntry> {
    for await(const expr of readExprStream(readUint8ArrayStream(path))) {
        const res = TubeFileEntry.codec.decode(expr);
        if(!res.success) {
            throw new Error("Could not decode expression as Argon VM IR: " + res.message);
        }

        yield res.value;
    }
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



