import type * as backendApi from "@argon-lang/js-backend-api";
import type { Option } from "@argon-lang/esexpr";
import type { JSBackendOutput, TestProgram } from "../options.js";
import type { PromiseWithError } from "@argon-lang/noble-idl-core/util";
import { ModuleResolution } from "./moduleResolution.js";
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
    addFile(path: string, contents: string): void;
    executeScript(code: string): void;
    executeModule(code: string): void;
    output(): string;
    close(): void;
}


class GraalTestExecutor<E> extends TestExecutorBase<E> {
    override async run(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): PromiseWithError<string, backendApi.TestExecutionException> {
        const moduleRes = buildModuleResolution(program, libraries);

        const exec: GraalJavaScriptExecutor = new (Java.type("dev.argon.backend.backends.js.GraalJavaScriptExecutor"))();
        try {
            buildExecutor(exec, moduleRes, program, libraries);
            exec.executeModule(mainModule.replaceAll(".js\"", ".js.mjs\""));
            return exec.output(); 
        }
        finally {
            exec.close();
        }

    }

}

function buildExecutor(exec: GraalJavaScriptExecutor, moduleRes: ModuleResolution, program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): void {

    function addProgram(prefix: string, program: TestProgram): void {
        exec.addFile(prefix + "package.json", JSON.stringify(program.packageJson));
        for(const mod of program.modules) {
            const path = prefix + mod.path.join("/") + ".mjs";
            const url = new URL(".", "file://" + path);
            const sourceCode = updateImports(moduleRes, url, mod.sourceCode);
            exec.addFile(path, astring.generate(sourceCode));
        }
    }

    addProgram("/test/", program);
    for(const lib of libraries.entries) {
        const prefix = "/test/node_modules/" + tubePackageName(lib.name) + "/";
        addProgram(prefix, lib.library);
    }

    for(const [path, content] of runtimePackage()) {
        const suffix = path.endsWith(".js") ? ".mjs" : "";

        exec.addFile("/test/node_modules/@argon-lang/runtime" + path + suffix, content);
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


