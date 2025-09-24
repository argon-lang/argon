import type { EnumInfo, EnumVariantInfo, FunctionInfo, InstanceInfo, MethodInfo, ModuleExportEntry, ModuleInfo, ModuleModel, ProgramModel, RecordFieldInfo, RecordInfo, TraitInfo, TubeInfo } from "./program-model.js";
import type { TubeHeader, TubeFileEntry, TubeMetadata, ImportSpecifier } from "@argon-lang/js-backend-api/vm";
import { getModuleId } from "./util.js";

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
    readonly recordFieldMap = new Map<bigint, (TubeFileEntry & { $type: "record-field-reference" | "enum-variant-record-field-reference" })>();
    readonly enumMap = new Map<bigint, (TubeFileEntry & { $type: "enum-reference" | "enum-definition" })>();
    readonly enumVariantMap = new Map<bigint, (TubeFileEntry & { $type: "enum-variant-reference" })>();
    readonly traitMap = new Map<bigint, (TubeFileEntry & { $type: "trait-reference" | "trait-definition" })>();
    readonly methodMap = new Map<bigint, (TubeFileEntry & { $type: "trait-method-reference" })>();
    readonly instanceMap = new Map<bigint, (TubeFileEntry & { $type: "instance-reference" | "instance-definition" })>();


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
            enumMap: this.enumMap,
            enumVariantMap: this.enumVariantMap,
            traitMap: this.traitMap,
            methodMap: this.methodMap,
            instanceMap: this.instanceMap,
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

        // Return early for references and methods.
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

            case "enum-definition":
                this.enumMap.set(entry.definition.enumId, entry);
                importSpec = entry.definition.import;
                exportEntry = entry;
                break;

            case "enum-reference":
                this.enumMap.set(entry.enumId, entry);
                return;

            case "enum-variant-reference":
                this.enumVariantMap.set(entry.variantId, entry);
                return;

            case "record-field-reference":
                this.recordFieldMap.set(entry.recordId, entry);
                return;

            case "enum-variant-record-field-reference":
                this.recordFieldMap.set(entry.recordFieldId, entry);
                return;

            case "trait-definition":
                this.traitMap.set(entry.definition.traitId, entry);
                importSpec = entry.definition.import;
                exportEntry = entry;
                break;

            case "trait-reference":
                this.traitMap.set(entry.traitId, entry);
                return;

            case "trait-method-reference":
                this.methodMap.set(entry.methodId, entry);
                return;

            case "instance-definition":
                this.instanceMap.set(entry.definition.instanceId, entry);
                importSpec = entry.definition.import;
                exportEntry = entry;
                break;

            case "instance-reference":
                this.instanceMap.set(entry.instanceId, entry);
                return;
        }

        const moduleId = getModuleId(importSpec);
        let moduleBuilder = this.modules.get(moduleId);
        if(moduleBuilder === undefined) {
            moduleBuilder = {
                exports: [],
            };
            this.modules.set(moduleId, moduleBuilder);
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
    readonly recordFieldMap: Map<bigint, (TubeFileEntry & { $type: "record-field-reference" | "enum-variant-record-field-reference" })>;
    readonly enumMap: Map<bigint, (TubeFileEntry & { $type: "enum-reference" | "enum-definition" })>;
    readonly enumVariantMap: Map<bigint, (TubeFileEntry & { $type: "enum-variant-reference" })>;
    readonly traitMap: Map<bigint, (TubeFileEntry & { $type: "trait-reference" | "trait-definition" })>;
    readonly methodMap: Map<bigint, (TubeFileEntry & { $type: "trait-method-reference" })>;
    readonly instanceMap: Map<bigint, (TubeFileEntry & { $type: "instance-reference" | "instance-definition" })>;
};

class ProgramModelImpl implements ProgramModel {
    constructor(options: ProgramModelOptions) {
        this.header = options.header;
        this.metadata = options.metadata;
        this.modules = options.modules;

        this.#moduleRefMap = options.moduleRefMap;
        this.#functionMap = options.functionMap;
        this.#recordMap = options.recordMap;
        this.#recordFieldMap = options.recordFieldMap;
        this.#enumMap = options.enumMap;
        this.#enumVariantMap = options.enumVariantMap;
        this.#traitMap = options.traitMap;
        this.#methodMap = options.methodMap;
        this.#instanceMap = options.instanceMap;
    }

    readonly header: TubeHeader;
    readonly metadata: TubeMetadata;
    readonly modules: readonly ModuleModel[];

    readonly #moduleRefMap: Map<bigint, TubeFileEntry & { $type: "module-reference" }>;
    readonly #functionMap: Map<bigint, (TubeFileEntry & { $type: "function-reference" | "function-definition" })>;
    readonly #recordMap: Map<bigint, (TubeFileEntry & { $type: "record-reference" | "record-definition" })>;
    readonly #recordFieldMap: Map<bigint, (TubeFileEntry & { $type: "record-field-reference" | "enum-variant-record-field-reference" })>;
    readonly #enumMap: Map<bigint, (TubeFileEntry & { $type: "enum-reference" | "enum-definition" })>;
    readonly #enumVariantMap: Map<bigint, (TubeFileEntry & { $type: "enum-variant-reference" })>;
    readonly #traitMap: Map<bigint, (TubeFileEntry & { $type: "trait-reference" | "trait-definition" })>;
    readonly #methodMap: Map<bigint, (TubeFileEntry & { $type: "trait-method-reference" })>;
    readonly #instanceMap: Map<bigint, (TubeFileEntry & { $type: "instance-reference" | "instance-definition" })>;

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
            const moduleRef = this.#moduleRefMap.get(id);
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

    getRecordInfo(id: bigint): RecordInfo {
        const entry = this.#recordMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid record id");
        }

        let importSpecifier: ImportSpecifier;
        if(entry.$type === "record-definition") {
            importSpecifier = entry.definition.import;
        }
        else {
            importSpecifier = entry.import;
        }

        return {
            importSpecifier,
        };
    }

    getRecordFieldInfo(id: bigint): RecordFieldInfo {
        const entry = this.#recordFieldMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid record field id");
        }

        let ownerType: RecordFieldInfo["ownerType"];
        let recordId: bigint;
        switch(entry.$type) {
            case "record-field-reference":
                ownerType = "record";
                recordId = entry.recordId;
                break;

            case "enum-variant-record-field-reference":
                ownerType = "enum-variant";
                recordId = entry.variantId;
                break;
        }

        return {
            ownerType,
            recordId: recordId,
            name: entry.name,
        };
    }

    getEnumInfo(id: bigint): EnumInfo {
        const entry = this.#enumMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid enum id");
        }

        let importSpecifier: ImportSpecifier;
        if(entry.$type === "enum-definition") {
            importSpecifier = entry.definition.import;
        }
        else {
            importSpecifier = entry.import;
        }

        return {
            importSpecifier,
        };
    }

    getEnumVariantInfo(id: bigint): EnumVariantInfo {
        const entry = this.#enumVariantMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid enum variant id");
        }

        return {
            enumId: entry.enumId,
            name: entry.name,
        };
    }

    getTraitInfo(id: bigint): TraitInfo {
        const entry = this.#traitMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid trait id");
        }

        let importSpecifier: ImportSpecifier;
        if(entry.$type === "trait-definition") {
            importSpecifier = entry.definition.import;
        }
        else {
            importSpecifier = entry.import;
        }

        return {
            importSpecifier,
        };
    }

    getMethodInfo(id: bigint): MethodInfo {
        const entry = this.#methodMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid trait id");
        }

        switch(entry.$type) {
            case "trait-method-reference":
            {
                const traitInfo = this.getTraitInfo(entry.traitId);

                return {
                    parentImportSpecifier: traitInfo.importSpecifier,
                    name: entry.name,
                    signature: entry.signature,
                };
            }
        }
    }

    getInstanceInfo(id: bigint): InstanceInfo {
        const entry = this.#instanceMap.get(id);
        if(entry === undefined) {
            throw new Error("Invalid instance id");
        }

        let importSpecifier: ImportSpecifier;
        if(entry.$type === "instance-definition") {
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


