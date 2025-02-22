import * as astring from "astring";
import * as acorn from "acorn";
import type * as estree from "estree";
import { ExternLoader } from "./externs.js";
import { TubeEmitter, type EmitOptions, type OutputModuleInfo } from "./emitter.js";
import { readIR } from "./ir-reader.js";
import type { Option } from "@argon-lang/esexpr";
import { getModuleOutputFileParts, getModulePathExternalUrl, getModulePathUrl, tubePackageName } from "./util.js";
import * as backendApi from "@argon-lang/js-backend-api";
import * as backendOptions from "@argon-lang/js-backend-api/options";
import type { OptionParser } from "@argon-lang/js-backend-api/options";
import type { ErrorChecker, PromiseWithError } from "@argon-lang/noble-idl-core/util";
import * as op from "@argon-lang/js-backend-api/options/parser";
import type { Dict } from "@argon-lang/noble-idl-core";
import { createTestExecutor } from "#executor";
import type { JSBackendOptions, JSBackendOutput, JSModuleResource, PackageJsonResource } from "./options.js";
import { asyncIterableToStream, streamToAsyncIterable } from "./stream.js";
import type { PackageJson, ReadonlyDeep } from "type-fest";


interface CodegenInput<E> {
    readonly options: JSBackendOptions<E>;
    readonly tubeInput: backendApi.VmIrTube<E>;
}

abstract class StringResourceBase<E> implements backendApi.BinaryResource<E> {

    abstract asString(): Promise<string>;

    async *asBytes(): AsyncIterable<Uint8Array> {
        const sourceCode = await this.asString();
        yield new TextEncoder().encode(sourceCode);
    }
    
}

class JSModuleResourceImpl<E> extends StringResourceBase<E> implements JSModuleResource<E> {
    constructor(public fileName: string, outputModule: OutputModuleInfo) {
        super();
        this.#outputModule = outputModule;
    }

    readonly #outputModule: OutputModuleInfo;

    async jsProgram(): Promise<estree.Program> {
        return this.#outputModule.emitJsProgram();
    }

    override async asString(): Promise<string> {
        const program = await this.jsProgram();
        return astring.generate(program);
    }
}

class PackageJsonResourceImpl<E> extends StringResourceBase<E> implements PackageJsonResource<E> {
    constructor(getEmitOptions: () => Promise<EmitOptions>) {
        super();
        this.#getEmitOptions = getEmitOptions;
    }

    readonly #getEmitOptions: () => Promise<EmitOptions>;

    async packageJson(): Promise<ReadonlyDeep<PackageJson>> {
        const emitOptions = await this.#getEmitOptions();
        return emitPackageJson(emitOptions)
    }

    override async asString(): Promise<string> {
        const packageJson = this.packageJson();
        return JSON.stringify(packageJson);
    }
}


async function loadEmitOptions<E>(input: CodegenInput<E>): Promise<EmitOptions> {
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

    return {
        program: programModel,
        externProvider: externLoader,
        tubeMapping: [],
    };
}

async function* emitModules<E>(options: EmitOptions): AsyncIterable<backendApi.DirectoryEntry<JSModuleResource<E>>> {
    const tubeEmitter = new TubeEmitter(options);
    
    for(const outputModule of tubeEmitter.emit()) {
        const outputFileParts = getModuleOutputFileParts(outputModule.modulePath);

        yield {
            dirs: outputFileParts[0],
            fileName: outputFileParts[1],
            resource: new JSModuleResourceImpl(outputFileParts[1], outputModule),
        };
    }
}

function emitPackageJson(options: EmitOptions): ReadonlyDeep<PackageJson> {
    const exports: PackageJson.ExportConditions = {};

    for(const module of options.program.modules) {
        const internalPath = getModulePathUrl(module.path);
        const externalPath = getModulePathExternalUrl(module.path);
        exports[externalPath === "" ? "." : "./" + externalPath] = "./" + internalPath;
    }


    return {
        name: tubePackageName(options.program.metadata.name),
        private: true,
        type: "module",
        exports,
    };
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
    #emitOptionsPromise: Promise<EmitOptions> | null = null;

    #getEmitOptions(): Promise<EmitOptions> {
        if(this.#emitOptionsPromise === null) {
            this.#emitOptionsPromise = loadEmitOptions({
                options: this.#options,
                tubeInput: this.#program,
            });
        }

        return this.#emitOptionsPromise;
    }

    get packageJson(): PackageJsonResource<E> {
        return new PackageJsonResourceImpl(() => this.#getEmitOptions());
    }

    get modules(): backendApi.DirectoryResource<E, JSModuleResource<E>> {
        const outputImpl = this;
        return {
            async contents() {
                const options = await outputImpl.#getEmitOptions();
                return await asyncIterableToStream(emitModules(options));
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



