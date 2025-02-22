import type * as backendApi from "@argon-lang/js-backend-api";
import type { Option } from "@argon-lang/esexpr";
import type { JSBackendOutput, TestProgram, TestProgramModule } from "../options.js";
import type { PromiseWithError } from "@argon-lang/noble-idl-core/util";
import { streamToAsyncIterable } from "../stream.js";
import { ModuleResolution } from "./moduleResolution.js";
import { tubePackageName } from "../util.js";
import type { ReadonlyDeep } from "type-fest";
import type * as estree from "estree";
import * as astring from "astring";

export function createTestExecutor<E>(): Option<backendApi.TestExecutorFactory<E, JSBackendOutput<E>>> {
    console.log("Create test executor");
    return {
        async create(callback) {
            return callback.call(new GraalTestExecutor());
        }
    };
}

interface JavaGlobal {
    type(name: string): any;
}

declare const Java: JavaGlobal;


interface GraalJavaScriptExecutor {
    addFile(path: readonly string[], contents: string): void;
    executeScript(code: string): void;
    executeModule(code: string): void;
    output(): string;
    close(): void;
}


class GraalTestExecutor<E> implements backendApi.TestExecutor<E, JSBackendOutput<E>, TestProgram> {
    async toTestProgram(program: JSBackendOutput<E>): PromiseWithError<TestProgram, E> {
        console.log("To test program: " + JSON.stringify(program.packageJson));
        const packageJson = await program.packageJson.packageJson();

        const modules: TestProgramModule[] = [];

        for await(const mod of streamToAsyncIterable(() => program.modules.contents())) {
            modules.push({
                path: [...mod.dirs, mod.fileName],
                sourceCode: await mod.resource.jsProgram(),
            });
        }

        return {
            packageJson,
            modules,
        };
    }


    async run(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): PromiseWithError<string, backendApi.TestExecutionException> {
        const moduleRes = buildModuleResolution(program, libraries);

        const exec: GraalJavaScriptExecutor = new (Java.type("dev.argon.backend.backends.js.GraalJavaScriptExecutor"))();
        try {
            buildExecutor(exec, moduleRes, program, libraries);
            exec.executeModule("import main from \"\"; await main();");   
            return exec.output(); 
        }
        finally {
            exec.close();
        }

    }

}

function buildModuleResolution(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): ModuleResolution {
    const moduleRes = new ModuleResolution();

    function addProgram(prefix: string, program: TestProgram): void {
        moduleRes.addPackageJsonFile(prefix + "/package.json", program.packageJson);
        for(const mod of program.modules) {
            moduleRes.addSourceFile(prefix + "/" + mod.path.join("/"), mod.sourceCode);
        }
    }

    addProgram("/test", program);
    for(const lib of libraries.entries) {
        const prefix = "/test/node_modules/" + tubePackageName(lib.name);
        addProgram(prefix, lib.library);
    }

    return moduleRes;
}

function buildExecutor(exec: GraalJavaScriptExecutor, moduleRes: ModuleResolution, program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): GraalJavaScriptExecutor {

    function addProgram(prefix: readonly string[], program: TestProgram): void {
        exec.addFile([ ...prefix, "package.json" ], JSON.stringify(program.packageJson));
        for(const mod of program.modules) {
            const pathParts = [ ...prefix, ...mod.path.slice(0, mod.path.length - 1), mod.path + ".mjs" ];
            const url = new URL("file:///" + pathParts.slice(mod.path.length - 1).join("/"));
            const sourceCode = updateImports(moduleRes, url, mod.sourceCode);
            exec.addFile(pathParts, astring.generate(sourceCode));
        }
    }

    addProgram([ "test" ], program);
    for(const lib of libraries.entries) {
        const prefix = [ "test", "node_modules", ...tubePackageName(lib.name) ];
        addProgram(prefix, lib.library);
    }

    return exec;
}

function updateImports(moduleRes: ModuleResolution, parentURL: URL, program: ReadonlyDeep<estree.Program>): ReadonlyDeep<estree.Program> {

    function fix(source: string): string {
        return moduleRes.esmResolve(source, parentURL).resolved.pathname;
    }

    return {
        ...program,
        body: program.body.map(stmt => {
            if(stmt.type === "ImportDeclaration" || stmt.type === "ExportNamedDeclaration") {
                if(stmt.source !== null && stmt.source !== undefined && typeof stmt.source.value === "string") {
                    stmt = {
                        ...stmt,
                        source: {
                            ...stmt.source,
                            value: typeof stmt.source.value === "string"
                                ? fix(stmt.source.value)
                                : stmt.source.value,
                        },
                    };
                }
            }

            return stmt;
        }),
    };
}


