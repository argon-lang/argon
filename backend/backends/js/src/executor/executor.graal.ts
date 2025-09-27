import type * as backendApi from "@argon-lang/js-backend-api";
import type { Option } from "@argon-lang/esexpr";
import type { JSBackendOutput, TestProgram } from "../options.js";
import type { PromiseWithError } from "@argon-lang/noble-idl-core/util";
import { ModuleResolution } from "@argon-lang/js-module-resolution";
import { tubePackageName } from "../util.js";
import type { ReadonlyDeep } from "type-fest";
import type * as estree from "estree";
import * as astring from "astring";
import { buildModuleResolution, mainModule, runtimePackage, TestExecutorBase } from "./executor-common.js";

export function createTestExecutor<E>(): Option<backendApi.TestExecutorFactory<E, JSBackendOutput<E>>> {
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
    executeScript(code: string): void;
    executeModule(code: string): void;
    output(): string;
    close(): void;
}

interface JavaMap<K, V> {
    put(k: K, v: V): V | null;
}


class GraalTestExecutor<E> extends TestExecutorBase<E> {
    override async run(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): PromiseWithError<string, backendApi.TestExecutionException> {
        const moduleRes = buildModuleResolution(program, libraries);

        const fileMap: JavaMap<string, string> = new (Java.type("java.util.HashMap"))();
        buildExecutor(fileMap, moduleRes, program, libraries);

        const exec: GraalJavaScriptExecutor = new (Java.type("dev.argon.backend.jsApi.GraalJavaScriptExecutor"))(fileMap);
        try {
            exec.executeModule(mainModule.replaceAll(".js\"", ".js.mjs\""));
            return exec.output(); 
        }
        finally {
            exec.close();
        }
    }

}

function buildExecutor(exec: JavaMap<string, string>, moduleRes: ModuleResolution, program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): void {

    function addProgram(prefix: string, program: TestProgram): void {
        exec.put(prefix + "package.json", JSON.stringify(program.packageJson));
        for(const mod of program.modules) {
            const path = prefix + mod.path.join("/") + ".mjs";
            const url = new URL(".", "file://" + path);
            const sourceCode = updateImports(moduleRes, url, mod.sourceCode);
            exec.put(path, astring.generate(sourceCode));
        }
    }

    addProgram("/test/", program);
    for(const lib of libraries.entries) {
        const prefix = "/test/node_modules/" + tubePackageName(lib.name) + "/";
        addProgram(prefix, lib.library);
    }

    for(const [path, content] of runtimePackage()) {
        const suffix = path.endsWith(".js") ? ".mjs" : "";

        exec.put("/test/node_modules/@argon-lang/runtime" + path + suffix, content);
    }
}

function updateImports(moduleRes: ModuleResolution, parentURL: URL, program: ReadonlyDeep<estree.Program>): ReadonlyDeep<estree.Program> {

    function fix(source: string): string {
        return moduleRes.esmResolve(source, parentURL).resolved.pathname + ".mjs";
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


