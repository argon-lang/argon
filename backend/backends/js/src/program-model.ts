import type { ModulePath, TubeFileEntry, TubeHeader, TubeMetadata, TubeName, ImportSpecifier } from "@argon-lang/js-backend-api/vm";

export interface ProgramModel {
    readonly header: TubeHeader;
    readonly metadata: TubeMetadata;
    readonly modules: readonly ModuleModel[];

    getTubeInfo(id: bigint): TubeInfo;
    getModuleInfo(id: bigint): ModuleInfo;
    getFunctionInfo(id: bigint): FunctionInfo;
}

export interface ModuleModel {
    readonly path: ModulePath;
    readonly exports: readonly ModuleExportEntry[];
}

export type ModuleExportEntry = TubeFileEntry & { $type: "function-definition" | "record-definition" };

export interface TubeInfo {
    readonly tubeName: TubeName;
}

export interface ModuleInfo {
    readonly tubeId: bigint;
    readonly path: ModulePath;
}

export interface FunctionInfo {
    readonly importSpecifier: ImportSpecifier;
}


