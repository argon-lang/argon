import type { TubeFormatVersion, Metadata, ModulePath, ModuleDefinition, ClassDefinition, TraitDefinition, FunctionDefinition, MethodDefinition, ClassConstructorDefinition } from "./proto/tube.js";
import type { FileSystemResource } from "./resource.js";

export interface SerializedTube {
    version(): Promise<TubeFormatVersion>;
    metadata(): Promise<Metadata>

    getResource(id: string): Promise<FileSystemResource>;

    getModule(modulePath: ModulePath): Promise<ModuleDefinition>;
    getClass(id: BigInt): Promise<ClassDefinition>;
    getTrait(id: BigInt): Promise<TraitDefinition>;
    getFunction(id: BigInt): Promise<FunctionDefinition>;
    getMethod(id: BigInt): Promise<MethodDefinition>;
    getClassConstructor(id: BigInt): Promise<ClassConstructorDefinition>;

    close(): Promise<void>;
}
