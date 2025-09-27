import * as backendApi from "@argon-lang/js-backend-api";
import type { JSBackendOutput, TestProgram, TestProgramModule } from "../options.js";
import type { PromiseWithError } from "@argon-lang/noble-idl-core/util";
import { streamToAsyncIterable } from "../stream.js";
import { ModuleResolution } from "@argon-lang/js-module-resolution";
import { tubePackageName } from "../util.js";
import argonRuntimePackage from "./argon-runtime.js";
import * as astring from "astring";

export abstract class TestExecutorBase<E> implements backendApi.TestExecutor<E, JSBackendOutput<E>, TestProgram> {
    async toTestProgram(program: JSBackendOutput<E>): PromiseWithError<TestProgram, E> {
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


    abstract run(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): PromiseWithError<string, backendApi.TestExecutionException>;
}


export function buildModuleResolution(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): ModuleResolution {
    const files = new Map<string, string>();

    function addProgram(prefix: string, program: TestProgram): void {
        files.set(prefix + "/package.json", JSON.stringify(program.packageJson));
        for(const mod of program.modules) {
            files.set(prefix + "/" + mod.path.join("/"), astring.generate(mod.sourceCode));
        }
    }

    addProgram("/test", program);
    for(const lib of libraries.entries) {
        const prefix = "/test/node_modules/" + tubePackageName(lib.name);
        addProgram(prefix, lib.library);
    }

    for(const [path, content] of runtimePackage()) {
        files.set("/test/node_modules/@argon-lang/runtime" + path, content);
    }

    return new ModuleResolution(files);
}

export const mainModule = "import { main$a$t$e$r$t$e } from \"/test/index.js\"; main$a$t$e$r$t$e();";


export function runtimePackage(): Map<string, string> {
    return new Map(Object.entries(argonRuntimePackage));
}

