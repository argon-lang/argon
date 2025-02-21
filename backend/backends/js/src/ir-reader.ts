import type { FunctionInfo, ModuleExportEntry, ModuleInfo, ModuleModel, ProgramModel, TubeInfo } from "./program-model.js";
import type { TubeHeader, TubeFileEntry, TubeMetadata, ImportSpecifier } from "@argon-lang/js-backend-api/vm";

export interface ElementLookup {
    getById(id: bigint): ElementResult;
}

export interface ElementResult {
    package: string;
    path: string;
    identifier: string;
}

class IrReader {

    header: TubeHeader | null = null;
    metadata: TubeMetadata | null = null;
    readonly modules = new Map<bigint, ModuleModelBuilder>();

    readonly moduleRefMap = new Map<bigint, TubeFileEntry & { $type: "module-reference" }>();
    readonly functionMap = new Map<bigint, (TubeFileEntry & { $type: "function-reference" | "function-definition" })>();
    readonly recordMap = new Map<bigint, (TubeFileEntry & { $type: "record-reference" | "record-definition" })>();
    readonly recordFieldMap = new Map<bigint, (TubeFileEntry & { $type: "record-field-reference" })>();


    build(): ProgramModel {
        if(this.header === null) {
            throw new Error("VMIR header is missing");
        }

        if(this.metadata === null) {
            throw new Error("VMIR metadata is missing");
        }

        return new ProgramModelImpl({
            header: this.header,
            metadata: this.metadata,
            modules: this.metadata.modules.map((mod, i) => {
                const moduleModel = this.modules.get(BigInt(i));

                return {
                    path: mod.path,
                    exports: moduleModel?.exports ?? [],
                };
            }),

            moduleRefMap: this.moduleRefMap,
            functionMap: this.functionMap,
            recordMap: this.recordMap,
            recordFieldMap: this.recordFieldMap,
        });
    }


    recordEntry(entry: TubeFileEntry): void {
        if(this.header === null) {
            if(entry.$type !== "header") {
                throw new Error("First VMIR entry must be a header");
            }

            this.header = entry.header;
            return;
        }
        
        if(this.metadata === null) {
            if(entry.$type !== "metadata") {
                throw new Error("Second VMIR entry must be metadata");
            }

            this.metadata = entry.metadata;
            return;
        }
        
        
        let importSpec: ImportSpecifier;
        let exportEntry: ModuleExportEntry;

        switch(entry.$type) {
            case "header":
                throw new Error("Extra header not allowed in VMIR");

            case "metadata":
                throw new Error("Extra metadata not allowed in VMIR");

            case "module-reference":
                this.moduleRefMap.set(entry.moduleId, entry);
                return;

            case "function-definition":
                this.functionMap.set(entry.definition.functionId, entry);
                importSpec = entry.definition.import;
                exportEntry = entry;
                break;

            case "function-reference":
                this.functionMap.set(entry.functionId, entry);
                return;

            case "record-definition":
                this.recordMap.set(entry.definition.recordId, entry);
                importSpec = entry.definition.import;
                exportEntry = entry;
                break;

            case "record-reference":
                this.recordMap.set(entry.recordId, entry);
                return;

            case "record-field-reference":
                this.recordFieldMap.set(entry.recordId, entry);
                return;
        }

        let moduleBuilder = this.modules.get(importSpec.moduleId);
        if(moduleBuilder === undefined) {
            moduleBuilder = {
                exports: [],
            };
            this.modules.set(importSpec.moduleId, moduleBuilder);
        }

        moduleBuilder.exports.push(exportEntry);
    }

}


interface ModuleModelBuilder {
    readonly exports: ModuleExportEntry[];
}


export async function readIR(entries: AsyncIterable<TubeFileEntry>): Promise<ProgramModel> {
    const reader = new IrReader();
    for await(const entry of entries) {
        reader.recordEntry(entry);
    }
    return reader.build();
}

type ProgramModelOptions = Pick<ProgramModel, "header" | "metadata" | "modules"> & {
    readonly moduleRefMap: Map<bigint, TubeFileEntry & { $type: "module-reference" }>;
    readonly functionMap: Map<bigint, (TubeFileEntry & { $type: "function-reference" | "function-definition" })>;
    readonly recordMap: Map<bigint, (TubeFileEntry & { $type: "record-reference" | "record-definition" })>;
    readonly recordFieldMap: Map<bigint, (TubeFileEntry & { $type: "record-field-reference" })>;
};

class ProgramModelImpl implements ProgramModel {
    constructor(options: ProgramModelOptions) {
        this.header = options.header;
        this.metadata = options.metadata;
        this.modules = options.modules;

        this.#moduleRefMap = options.moduleRefMap;
        this.#functionMap = options.functionMap;
        // this.#recordMap = options.recordMap;
        // this.#recordFieldMap = options.recordFieldMap;
    }

    readonly header: TubeHeader;
    readonly metadata: TubeMetadata;
    readonly modules: readonly ModuleModel[];

    readonly #moduleRefMap: Map<bigint, TubeFileEntry & { $type: "module-reference" }>;
    readonly #functionMap: Map<bigint, (TubeFileEntry & { $type: "function-reference" | "function-definition" })>;
    // readonly #recordMap: Map<bigint, (TubeFileEntry & { $type: "record-reference" | "record-definition" })>;
    // readonly #recordFieldMap: Map<bigint, (TubeFileEntry & { $type: "record-field-reference" })>;

    getTubeInfo(id: bigint): TubeInfo {
        if(id === 0n) {
            return {
                tubeName: this.metadata.name,
            };
        }
        else {
            const tubeName = this.metadata.referencedTubes[Number(id - 1n)];
            if(tubeName === undefined) {
                throw new Error("Invalid tube id");
            }

            return {
                tubeName,
            };
        }
    }

    getModuleInfo(id: bigint): ModuleInfo {
        if(id < BigInt(this.metadata.modules.length)) {
            const module = this.metadata.modules[Number(id)];
            if(module === undefined) {
                throw new Error("Could not get declared module");
            }

            return {
                path: module.path,
                tubeId: 0n,
            };
        }
        else {
            const moduleRef = this.#moduleRefMap.get(id - BigInt(this.metadata.modules.length));
            if(moduleRef === undefined) {
                throw new Error("Could not get referenced module");
            }

            return {
                path: moduleRef.path,
                tubeId: moduleRef.tubeId,
            };
        }
    }

    getFunctionInfo(id: bigint): FunctionInfo {
        const entry = this.#functionMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid function id");
        }

        let importSpecifier: ImportSpecifier;
        if(entry.$type === "function-definition") {
            importSpecifier = entry.definition.import;
        }
        else {
            importSpecifier = entry.import;
        }

        return {
            importSpecifier,
        };
    }
}


