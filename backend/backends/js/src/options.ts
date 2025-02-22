import * as backendApi from "@argon-lang/js-backend-api";
import type * as estree from "estree";
import type { PackageJson, ReadonlyDeep } from "type-fest";

export interface JSBackendOptions<E> {
    readonly externs: readonly backendApi.BinaryResource<E>[];
}

export interface JSBackendOutput<E> {
    readonly packageJson: PackageJsonResource<E>;
    readonly modules: backendApi.DirectoryResource<E, JSModuleResource<E>>;
}

export interface JSModuleResource<E> extends backendApi.BinaryResource<E> {
    jsProgram(): Promise<ReadonlyDeep<estree.Program>>;
    asString(): Promise<string>;
}

export interface PackageJsonResource<E> extends backendApi.BinaryResource<E> {
    packageJson(): Promise<ReadonlyDeep<PackageJson>>;
    asString(): Promise<string>;
}


export interface TestProgram {
    readonly packageJson: ReadonlyDeep<PackageJson>;
    readonly modules: readonly TestProgramModule[];
}

export interface TestProgramModule {
    readonly path: readonly string[];
    readonly sourceCode: ReadonlyDeep<estree.Program>;
}




