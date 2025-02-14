import * as astring from "astring";
import * as acorn from "acorn";
import type * as estree from "estree";
import { ExternLoader } from "./externs.js";
import * as ir from "./vm-format.js";
import { TubeEmitter, type EmitOptions, type TubeMapping } from "./emitter.js";
import { readIR } from "./ir-reader.js";
import type { ESExpr } from "@argon-lang/esexpr";
import { readExprStream } from "@argon-lang/esexpr/binary_format";
import { getModuleOutputFileParts } from "./util.js";

export interface CodegenInput {
    readonly tubeMapping: readonly TubeMapping[];
    readonly tubeInput: TubeInput;
    getExterns(): AsyncIterable<ExternsInfo>;
}

export type TubeInput =
    | { type: "ir", entries(): AsyncIterable<ir.TubeFileEntry> }
    | { type: "ir-expr", exprs(): AsyncIterable<ESExpr> }
    | { type: "ir-encoded", data(): AsyncIterable<Uint8Array> }
;

export interface ExternsInfo {
    readonly sourceCode: string;
    readonly sourceFile: string;
}

export interface ModuleCodegenResult {
    readonly moduleFilePath: readonly string[];
    readonly sourceCode: string;
}

export async function* codegen(input: CodegenInput): AsyncIterable<ModuleCodegenResult> {

    const externLoader = new ExternLoader();

    for await(const extern of input.getExterns()) {
        const program: acorn.Program = acorn.parse(extern.sourceCode, {
            ecmaVersion: 2024,
            sourceType: "module",
            sourceFile: extern.sourceFile,
            allowHashBang: false,
        });
        externLoader.addExterns(program as estree.Program);
    }
    
    let ir: AsyncIterable<ir.TubeFileEntry>;
    switch(input.tubeInput.type) {
        case "ir":
            ir = input.tubeInput.entries();
            break;

        case "ir-expr":
            ir = decodeIrEntries(input.tubeInput.exprs());
            break;

        case "ir-encoded":
            ir = decodeIrEntries(readExprStream(input.tubeInput.data()));
            break;
    }
    
    const programModel = await readIR(ir);
    
    const options: EmitOptions = {
        program: programModel,
        externProvider: externLoader,
        tubeMapping: input.tubeMapping,
    };
    
    const tubeEmitter = new TubeEmitter(options);
    
    for(const emittedModule of tubeEmitter.emit()) {
        const sourceCode = astring.generate(emittedModule.jsProgram);
        yield {
            moduleFilePath: getModuleOutputFileParts(emittedModule.modulePath),
            sourceCode,
        };
    }
}

export interface TestProgram {
    readonly packageJson: string,
    readonly modules: readonly ModuleCodegenResult[],
}

export interface TestLibrary {
    readonly name: ir.TubeName;
    readonly program: TestProgram;
}

export interface TestInput {
    readonly program: TestProgram;
}

export async function runTest(_input: TestInput): Promise<string> {
    throw new Error("Not implemented");
}



async function* decodeIrEntries(exprs: AsyncIterable<ESExpr>): AsyncIterable<ir.TubeFileEntry> {
    for await(const expr of exprs) {
        const res = ir.TubeFileEntry.codec.decode(expr);
        if(!res.success) {
            throw new Error("Could not decode expression as Argon VM IR: " + res.message);
        }

        yield res.value;
    }
}
