import { ModuleResolution } from "@argon-lang/js-module-resolution";
import * as acorn from "acorn";
import * as astring from "astring";
import type * as estree from "estree";

export type FileMap = {
    readonly [key: string]: string;
}

export function resolveImports(entrypoint: string, files: FileMap): FileMap {
    const filesMap = new Map(Object.entries(files));

    const processor = new FileProcessor(filesMap);

    processor.observeFile(entrypoint);
    processor.run();

    return Object.fromEntries(processor.updatedFiles.entries());
}

class FileProcessor {

    constructor(files: ReadonlyMap<string, string>) {
        this.#files = files;
    }

    readonly #files: ReadonlyMap<string, string>;
    readonly #unprocessedFiles = new Set<string>();
    readonly updatedFiles = new Map<string, string>();

    observeFile(file: string) {
        if(!this.updatedFiles.has(file)) {
            this.#unprocessedFiles.add(file);
        }
    }

    run() {
        for(;;) {
            const [file] = this.#unprocessedFiles;
            if(file === undefined) {
                break;
            }

            this.#processFile(file);
            this.#unprocessedFiles.delete(file);
        }
    }

    #processFile(file: string) {
        const moduleRes = new ModuleResolution(this.#files);

        const fileUrl = new URL(".", "file://" + file);

        const jsCodeStr = this.#files.get(file);
        if(jsCodeStr === undefined) {
            throw new Error("Could not find file: " + file);
        }

        const jsCode = acorn.parse(jsCodeStr, {
            ecmaVersion: "latest",
            sourceType: "module",
        }) as estree.Program;

        for(const stmt of jsCode.body) {
            if(stmt.type !== "ImportDeclaration" && stmt.type !== "ExportNamedDeclaration") {
                continue;
            }

            if(!stmt.source) {
                continue;
            }


            if(typeof(stmt.source.value) !== "string") {
                throw new Error("Import source must be a string in " + file);
            }

            const resolution = moduleRes.esmResolve(stmt.source.value, fileUrl);

            const importedFile = resolution.resolved.pathname;
            this.observeFile(importedFile);
            stmt.source = {
                type: "Literal",
                value: importedFile,
            };
        }

        this.updatedFiles.set(file, astring.generate(jsCode));
        this.#unprocessedFiles.delete(file);
    }

}


