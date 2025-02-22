import * as backendApi from "@argon-lang/js-backend-api";
import type { JSBackendOutput, TestProgram, TestProgramModule } from "../options.js";
import type { PromiseWithError } from "@argon-lang/noble-idl-core/util";
import { streamToAsyncIterable } from "../stream.js";
import { ModuleResolution } from "./moduleResolution.js";
import { tubePackageName } from "../util.js";

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

export const mainModule = "import { main$a$t$e$r$t$e } from \"/test/index.js\"; main$a$t$e$r$t$e();";

