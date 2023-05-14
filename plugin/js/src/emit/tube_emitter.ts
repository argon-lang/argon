import type { DirectoryEntry, DirectoryResource } from "@argon-lang/plugin-api/resource";
import type { EmitTubeInfo, Metadata, ModuleName, ModulePath, TubeName } from "@argon-lang/plugin-api/tube";
import type { Program } from "estree";
import type { ExternImpl } from "../extern.js";
import type { JSOptions, JSOutput } from "../options.js";
import { JSProgramResource } from "../resource.js";
import { EmitTubeCommon } from "./emit_tube_common.js";
import { ModuleEmitter } from "./module_emitter.js";
import { getModuleFileName, ModuleFile } from "./path.js";



class TubeDirectoryStructure implements DirectoryResource<JSProgramResource> {
    constructor(files: () => Promise<readonly ModuleFile[]>, emitModule: (modulePath: ModulePath) => JSProgramResource) {
        this.#files = files;
        this.#emitModule = emitModule;
    }

    readonly #files: () => Promise<readonly ModuleFile[]>;
    readonly #emitModule: (modulePath: ModulePath) => JSProgramResource;

    resourceType: "directory" = "directory";

    get fileName(): string | null {
        return null;
    }

    async *contents(): AsyncIterable<DirectoryEntry<JSProgramResource>> {
        const subdirs = new Map<string, ModuleFile[]>();
        for(const file of await this.#files()) {
            const [ subdir, ...restPath ] = file.dir;
            if(subdir !== undefined) {
                let subdirFiles = subdirs.get(subdir);
                if(subdirFiles === undefined) {
                    subdirFiles = [];
                    subdirs.set(subdir, subdirFiles)
                }

                subdirFiles.push({
                    dir: restPath,
                    file: file.file,
                    path: file.path,
                });
            }
            else {
                yield {
                    entryType: "file",
                    name: file.file,
                    resource: this.#emitModule(file.path),
                };
            }
        }

        for(const [dirName, files] of subdirs) {
            yield {
                entryType: "subdirectory",
                name: dirName,
                resource: new TubeDirectoryStructure(async () => files, this.#emitModule),
            };
        }
    }

}

class TubeModuleResource extends JSProgramResource {
    constructor(emitter: TubeEmitter, path: ModulePath) {
        super();
        this.#emitter = emitter;
        this.#path = path;
    }

    readonly #emitter: TubeEmitter;
    readonly #path: ModulePath;

    get fileName(): string | null {
        return null;
    }

    async asModule(): Promise<Program> {
        const moduleDef = await this.#emitter.tube.getModule(this.#path);
        const metadata = this.#emitter.metadata;
        const moduleName: ModuleName = {
            tube: metadata.name,
            path: this.#path,
        };
        const moduleEmitter = new ModuleEmitter(this.#emitter.options, this.#emitter.emitTubeInfo, metadata, moduleName, moduleDef);

        return await moduleEmitter.program();
    }

}

export class TubeEmitter extends EmitTubeCommon {
    constructor(options: JSOptions, emitInfo: EmitTubeInfo<ExternImpl, ExternImpl, ExternImpl>, metadata: Metadata) {
        super(options, emitInfo, metadata);
    }

    async emitTube(): Promise<JSOutput> {
        return {
            package: this.#toDirectoryStructure(path => this.#emitModule(path)),
        };
    }

    #emitModule(path: ModulePath): JSProgramResource {
        return new TubeModuleResource(this, path);
    }

    #toDirectoryStructure(f: (modulePath: ModulePath) => JSProgramResource): DirectoryResource<JSProgramResource> {
        return new TubeDirectoryStructure(async () => (await this.tube.allModulePaths()).map(getModuleFileName), f);
    }

}
