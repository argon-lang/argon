import type * as backendApi from "@argon-lang/js-backend-api";
import type { Option } from "@argon-lang/esexpr";
import type { JSBackendOutput, TestProgram } from "../options.js";
import { buildModuleResolution, mainModule, runtimePackage, TestExecutorBase } from "./executor-common.js";
import type { PromiseWithError } from "@argon-lang/noble-idl-core/util";
import type { ModuleResolution } from "./moduleResolution.js";

import * as astring from "astring";
import { tubePackageName } from "../util.js";
import * as vm from "node:vm";
import { Console } from "node:console";
import { Writable } from "node:stream";
import type { ImportAttributes } from "node:module";

export function createTestExecutor<E>(): Option<backendApi.TestExecutorFactory<E, JSBackendOutput<E>>> {
    return {
        async create(callback) {
            return callback.call(new VMTestExecutor());
        }
    };
}

class VMTestExecutor<E> extends TestExecutorBase<E> {
    override async run(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): PromiseWithError<string, backendApi.TestExecutionException> {
        const moduleRes = buildModuleResolution(program, libraries);
        const fileMap = buildFileMap(program, libraries);
        return await runInVm(moduleRes, fileMap);
    }

}

function buildFileMap(program: TestProgram, libraries: backendApi.LibraryMap<TestProgram>): Map<string, string> {
    var map = new Map<string, string>();

    function addProgram(prefix: string, program: TestProgram): void {
        map.set(prefix + "package.json", JSON.stringify(program.packageJson));
        for(const mod of program.modules) {
            const path = prefix + mod.path.join("/");
            map.set(path, astring.generate(mod.sourceCode));
        }
    }

    addProgram("/test/", program);
    for(const lib of libraries.entries) {
        const prefix = "/test/node_modules/" + tubePackageName(lib.name) + "/";
        addProgram(prefix, lib.library);
    }

    for(const [path, content] of runtimePackage()) {
        map.set("/test/node_modules/@argon-lang/runtime" + path, content);
    }

    return map;

}

async function runInVm(moduleRes: ModuleResolution, fileMap: Map<string, string>): Promise<string> {
    const context = vm.createContext();

    const consoleOutput = new StringWritable();
    const virtConsole = new Console(consoleOutput);

    const setupConsole: (outerConsole: Console) => void = vm.runInContext(
        `outerConsole => {
            const o = {};
            for(const name in outerConsole) {
                o[name] = function(...args) {
                    outerConsole[name](...args);
                };
            }
            globalThis.console = o;
        }`,
        context
    );

    setupConsole(virtConsole);

    const modMap = new Map<string, vm.Module>();
    const modUrlMap = new Map<vm.Module, URL>();

    function importFrom(specifier: string, sourceURL: URL, importAttributes: ImportAttributes): vm.Module {
        if(importAttributes.type !== undefined && importAttributes.type !== "javascript-or-wasm") {
            throw new Error("Unsupported module type: " + importAttributes.type);
        }

        const { resolved, format } = moduleRes.esmResolve(specifier, new URL(".", sourceURL));
        if(format !== "module") {
            throw new Error("Found a file that is not a module");
        }

        let mod = modMap.get(resolved.pathname);
        if(mod === undefined) {
            mod = loadModule(resolved);
        }

        return mod;
    }

    function loadModule(url: URL): vm.Module {
        const path = url.pathname;
        const sourceCode = fileMap.get(path);
        if(sourceCode === undefined) {
            throw new Error("Could not find module: " + url);
        }

        const mod = new vm.SourceTextModule(sourceCode, {
            context,
            identifier: path,
            initializeImportMeta(meta: ImportMeta, _module: vm.SourceTextModule): void {
                meta.url = url.toString();
            },
            importModuleDynamically(specifier: string, _referrer: vm.SourceTextModule, importAttributes: ImportAttributes): vm.Module {
                return importFrom(specifier, url, importAttributes);
            },
        });

        modMap.set(path, mod);
        modUrlMap.set(mod, url);

        return mod;
    }

    const mainModuleSTM = new vm.SourceTextModule(mainModule, {
        context,
        identifier: "/test/main.js",
    });
    modMap.set("/test/main.js", mainModuleSTM);
    modUrlMap.set(mainModuleSTM, new URL("file:///test/main.js"));

    await mainModuleSTM.link(async (specifier, referencingModule, extra) => {
        const url = modUrlMap.get(referencingModule);
        if(url === undefined) {
            throw new Error("Could not find module url");
        }

        return importFrom(specifier, url, extra.attributes);
    });

    await mainModuleSTM.evaluate();

    return consoleOutput.output.join("");
}

class StringWritable extends Writable {
    output: string[] = [];

    override _write(chunk: unknown, _encoding: BufferEncoding, callback: (error?: Error | null) => void): void {
        let s: string;
        try {
            if(Buffer.isBuffer(chunk)) {
                s = chunk.toString("utf-8");
            }
            else if(typeof chunk !== "string") {
                s = String(chunk);
            }
            else {
                s = chunk;
            }
        }
        catch(e: any) {
            return callback(e);
        }
        this.output.push(s);
        callback();
    }
}

