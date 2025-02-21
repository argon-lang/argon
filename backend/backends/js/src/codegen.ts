import * as astring from "astring";
import * as acorn from "acorn";
import type * as estree from "estree";
import { ExternLoader } from "./externs.js";
import * as ir from "@argon-lang/js-backend-api/vm";
import { TubeEmitter, type EmitOptions, type TubeMapping } from "./emitter.js";
import { readIR } from "./ir-reader.js";
import type { ESExpr, Option } from "@argon-lang/esexpr";
import { readExprStream } from "@argon-lang/esexpr/binary_format";
import { getModuleOutputFileParts } from "./util.js";
import * as backendApi from "@argon-lang/js-backend-api";
import * as backendOptions from "@argon-lang/js-backend-api/options";
import type { OptionParser } from "@argon-lang/js-backend-api/options";
import type { ErrorChecker, PromiseWithError } from "@argon-lang/noble-idl-core/util";
import * as op from "@argon-lang/js-backend-api/options/parser";
import type { Dict } from "@argon-lang/noble-idl-core";
import { createTestExecutor } from "#executor";
import type { JSBackendOptions, JSBackendOutput } from "./options.js";

export interface CodegenInput {
    readonly tubeMapping: readonly TubeMapping[];
    readonly tubeInput: TubeInput;
    getExterns(): AsyncIterable<ExternsInfo>;
}


export interface CodegenInput2<E> {
    readonly options: JSBackendOptions<E>;
    readonly tubeInput: backendApi.VmIrTube<E>;
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
    
    for(const outputModule of tubeEmitter.emit()) {
        const program = outputModule.emitJsProgram();
        const sourceCode = astring.generate(program);
        const outputFileParts = getModuleOutputFileParts(outputModule.modulePath);
        const moduleFilePath = [ ...outputFileParts[0], outputFileParts[1] ];

        yield {
            moduleFilePath,
            sourceCode,
        };
    }
}


export async function* codegen2<E>(input: CodegenInput2<E>): AsyncIterable<backendApi.DirectoryEntry<E>> {

    const externLoader = new ExternLoader();

    for(const extern of input.options.externs) {
        const sourceCode = await resourceAsString(extern);
        const program: acorn.Program = acorn.parse(sourceCode, {
            ecmaVersion: 2024,
            sourceType: "module",
            sourceFile: extern.fileName ?? "<extern>.js",
            allowHashBang: false,
        });
        externLoader.addExterns(program as estree.Program);
    }
    
    let ir = streamToAsyncIterable(() => input.tubeInput.stream());
    
    const programModel = await readIR(ir);
    
    const options: EmitOptions = {
        program: programModel,
        externProvider: externLoader,
        tubeMapping: [],
    };
    
    const tubeEmitter = new TubeEmitter(options);
    
    for(const outputModule of tubeEmitter.emit()) {
        const outputFileParts = getModuleOutputFileParts(outputModule.modulePath);

        yield {
            dirs: outputFileParts[0],
            fileName: outputFileParts[1],
            resource: stringFuncAsResource(async () => {
                const program = outputModule.emitJsProgram();
                return astring.generate(program);
                
            }),
        };
    }
}

async function resourceAsString<E>(res: backendApi.BinaryResource<E>): PromiseWithError<string, E> {
    let s = "";
    const decoder = new TextDecoder();
    for await(const b of res.asBytes()) {
        s += decoder.decode(b, { stream: true });
    }
    s += decoder.decode();
    return s;
}

function stringFuncAsResource<E>(s: () => Promise<string>): backendApi.BinaryResource<E> {
    return {
        async *asBytes() {
            yield new TextEncoder().encode(await s());
        },
    };
}

async function* streamToAsyncIterable<E, A>(streamFunc: () => Promise<backendApi.ScopedResource<backendApi.Stream<E, A>>>): AsyncIterable<A> {
    const streamRes = await streamFunc();
    try {
        const stream = await streamRes.get();
        while(true) {
            const entries = await stream.next();
            if(entries.length === 0) {
                break;
            }

            yield* entries;
        }
    }
    finally {
        await streamRes.close();
    }
}


async function asyncIterableToStream<E, A>(iterable: AsyncIterable<A>): Promise<backendApi.ScopedResource<backendApi.Stream<E, A>>> {
    const iter = iterable[Symbol.asyncIterator]();

    let isDone = false;
    const stream: backendApi.Stream<E, A> = {
        async next() {
            const res = await iter.next();
            if(res.done) {
                return [];
            }

            return [ res.value ];
        },
    };

    return {
        async get() {
            return stream;
        },

        async close() {
            if(!isDone && iter.return) {
                await iter.return();
            }
        },
    };
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

class JSBackend<E> implements backendApi.Backend<E, JSBackendOutput<E>> {
    async codeGenerator(): Promise<backendApi.CodeGeneratorFactory<E, JSBackendOutput<E>>> {
        return new JSCodeGeneratorFactory();
    }
    
    async testExecutor(): Promise<Option<backendApi.TestExecutorFactory<E, JSBackendOutput<E>>>> {
        return createTestExecutor();
    }
}

class JSCodeGeneratorFactory<E> implements backendApi.CodeGeneratorFactory<E, JSBackendOutput<E>> {
    create<A>(callback: backendApi.CodeGeneratorFactoryCallback<E, JSBackendOutput<E>, A>): Promise<A> {
        return callback.call({
            $type: "library",
            generator: new JSCodeGenerator(),
        });
    }

}

class JSCodeGenerator<E> implements backendApi.LibraryCodeGenerator<E, JSBackendOptions<E>, JSBackendOutput<E>> {
    async optionParser(): Promise<OptionParser<E, JSBackendOptions<E>>> {
        return new JSOptionParser();
    }

    async codegen(options: JSBackendOptions<E>, program: backendApi.VmIrTube<E>, libraries: readonly backendApi.VmIrTube<E>[]): PromiseWithError<JSBackendOutput<E>, E> {
        return new JSBackendOutputImpl(options, program, libraries);
    }

    async outputProvider(): Promise<backendOptions.OutputProvider<E, JSBackendOutput<E>>> {
        return new JSOutputProvider();
    }
}

class JSBackendOutputImpl<E> implements JSBackendOutput<E> {
    constructor(options: JSBackendOptions<E>, program: backendApi.VmIrTube<E>, _libraries: readonly backendApi.VmIrTube<E>[]) {
        this.#options = options;
        this.#program = program;
    }

    readonly #options: JSBackendOptions<E>;
    readonly #program: backendApi.VmIrTube<E>;

    get modules(): backendApi.DirectoryResource<E> {
        const outputImpl = this;
        return {
            contents() {
                return asyncIterableToStream(codegen2({
                    options: outputImpl.#options,
                    tubeInput: outputImpl.#program,
                }));
            },
        };
    }
}

class JSOptionParser<E> implements backendOptions.OptionParser<E, JSBackendOptions<E>> {
    async parse(options: ReadonlyMap<string, backendOptions.OptionValue<E>>): PromiseWithError<JSBackendOptions<E>, backendOptions.OptionParseFailure> {
        return op.parse<E, JSBackendOptions<E>>(options, {
            externs: op.many(op.binaryResourceOption()),
        });
    }
}

class JSOutputProvider<E> implements backendOptions.OutputProvider<E, JSBackendOutput<E>> {
    async resources(o: JSBackendOutput<E>): Promise<Dict<backendOptions.OutputValue<E>>> {
        return new Map([
            [ "modules", { $type: "directory-resource", res: o.modules } ],
        ]);
    }
}



export interface HostOperations<_E> {

}

export interface BackendFactory {
    create<E, A>(_errorChecker_e: ErrorChecker<E>, _hostOperations: HostOperations<E>, f: <Output>(backend: backendApi.Backend<E, Output>) => A): A;
}

export const backendFactory: BackendFactory = {
    create<E, A>(_errorChecker_e: ErrorChecker<E>, _hostOperations: HostOperations<E>, f: <Output>(backend: backendApi.Backend<E, Output>) => A): A {
        return f(new JSBackend());
    },
};



