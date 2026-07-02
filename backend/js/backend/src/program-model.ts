import type { ModulePath, TubeFileEntry, TubeHeader, TubeMetadata, TubeName, ImportSpecifier, Identifier, ErasedSignature, MethodDefinition } from "@argon-lang/js-backend-api/vm.js";
import type {JsPlatformTubeMetadata} from "@argon-lang/js-backend-api";

export interface ProgramModel {
    readonly header: TubeHeader;
    readonly metadata: TubeMetadata;
    readonly decodedMetadata: DecodedMetadata;
    readonly modules: readonly ModuleModel[];

    getTubeInfo(id: bigint): TubeInfo;
    getModuleInfo(id: bigint): ModuleInfo;
    getFunctionInfo(id: bigint): FunctionInfo;
    getRecordInfo(id: bigint): RecordInfo;
    getRecordFieldInfo(id: bigint): RecordFieldInfo;
    getEnumInfo(id: bigint): EnumInfo;
    getEnumVariantInfo(id: bigint): EnumVariantInfo;
    getTraitInfo(id: bigint): TraitInfo;
    getMethodInfo(id: bigint): MethodInfo;
    getInstanceInfo(id: bigint): InstanceInfo;
}

export interface DecodedMetadata {
    platformMetadata: JsPlatformTubeMetadata;

    referencePlatformMetadata: readonly {
        tubeName: TubeName,
        platformMetadata: JsPlatformTubeMetadata,
    }[];
}

export interface ModuleModel {
    readonly path: ModulePath;
    readonly exports: readonly ModuleExportEntry[];
}

export type ModuleExportEntry = TubeFileEntry & { $type: "function-definition" | "record-definition" | "enum-definition" | "trait-definition" | "instance-definition" };

export interface TubeInfo {
    readonly tubeName: TubeName;
    readonly platformMetadata: JsPlatformTubeMetadata;
}

export interface ModuleInfo {
    readonly tubeId: bigint;
    readonly path: ModulePath;
}

export interface FunctionInfo {
    readonly importSpecifier: ImportSpecifier;
}

export interface RecordInfo {
    readonly importSpecifier: ImportSpecifier;
}

export interface RecordFieldInfo {
    readonly ownerType: "record" | "enum-variant";
    readonly recordId: bigint;
    readonly name: Identifier;
}

export interface EnumInfo {
    readonly importSpecifier: ImportSpecifier;
}

export interface EnumVariantInfo {
    readonly enumId: bigint;
    readonly name: Identifier;
}

export interface TraitInfo {
    readonly importSpecifier: ImportSpecifier;
}

export interface MethodInfo {
    readonly parentImportSpecifier: ImportSpecifier;
    readonly name: Identifier;
    readonly signature: ErasedSignature;

    readonly definition?: MethodDefinition | undefined;
}

export interface InstanceInfo {
    readonly importSpecifier: ImportSpecifier;
}

