import type * as tube from "./proto/tube.js";
import type { VTable } from "./proto/vtable.js";
import type { FileSystemResource } from "./resource.js";


export interface SerializedTube {
    version(): Promise<tube.TubeFormatVersion>;
    metadata(): Promise<tube.Metadata>

    getResource(id: string): Promise<FileSystemResource>;

    getModule(id: bigint): Promise<tube.ModuleDefinition>;
    getClass(id: bigint): Promise<tube.ClassDefinition>;
    getTrait(id: bigint): Promise<tube.TraitDefinition>;
    getFunction(id: bigint): Promise<tube.FunctionDefinition>;
    getMethod(id: bigint): Promise<tube.MethodDefinition>;
    getClassConstructor(id: bigint): Promise<tube.ClassConstructorDefinition>;


    close(): Promise<void>;
}

export interface SerializedTubePlus<ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation> extends SerializedTube {
    getExternMethodImplementation(id: bigint): Promise<ExternMethodImplementation>;
    getExternFunctionImplementation(id: bigint): Promise<ExternFunctionImplementation>;
    getExternClassConstructorImplementation(id: bigint): Promise<ExternClassConstructorImplementation>;

    getVTableDiff(id: bigint): Promise<VTable>;
}



