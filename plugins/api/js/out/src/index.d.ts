import * as Metadata from "./tube/Metadata.js";
import * as ModuleDeclaration from "./tube/ModuleDeclaration.js";
import * as ModuleDefinition from "./tube/ModuleDefinition.js";
import * as TraitReference from "./tube/TraitReference.js";
import * as TraitDefinition from "./tube/TraitDefinition.js";
import * as ClassReference from "./tube/ClassReference.js";
import * as ClassDefinition from "./tube/ClassDefinition.js";
import * as FunctionReference from "./tube/FunctionReference.js";
import * as FunctionDefinition from "./tube/FunctionDefinition.js";
import * as MethodReference from "./tube/MethodReference.js";
import * as MethodDefinition from "./tube/MethodDefinition.js";
import * as ClassConstructorReference from "./tube/ClassConstructorReference.js";
import * as ClassConstructorDefinition from "./tube/ClassConstructorDefinition.js";
export interface SerializedTube {
    metadata(): Promise<Metadata.V1>;
    moduleDeclaration(id: bigint): Promise<ModuleDeclaration.V1>;
    moduleDefinition(id: bigint): Promise<ModuleDefinition.V1>;
    traitRef(id: bigint): Promise<TraitReference.V1>;
    traitDef(id: bigint): Promise<TraitDefinition.V1>;
    classRef(id: bigint): Promise<ClassReference.V1>;
    classDef(id: bigint): Promise<ClassDefinition.V1>;
    functionRef(id: bigint): Promise<FunctionReference.V1>;
    functionDef(id: bigint): Promise<FunctionDefinition.V1>;
    externFunctionImplementation<ExternFunction, ExternMethod>(id: bigint, platform: Platform<ExternFunction, ExternMethod>): ExternFunction;
    methodRef(id: bigint): Promise<MethodReference.V1>;
    methodDef(id: bigint): Promise<MethodDefinition.V1>;
    externMethodImplementation<ExternFunction, ExternMethod>(id: bigint, platform: Platform<ExternFunction, ExternMethod>): ExternMethod;
    classConstructorRef(id: bigint): Promise<ClassConstructorReference.V1>;
    classConstructorDef(id: bigint): Promise<ClassConstructorDefinition.V1>;
}
export interface Platform<ExternFunction, ExternMethod> {
    readonly id: String;
    readonly name: String;
    readonly externFunctionCodec: ExternCodec<ExternFunction>;
    readonly externMethodCodec: ExternCodec<ExternMethod>;
}
export interface ExternCodec<T> {
    decode(resource: BinaryResource): Promise<T>;
    encode(value: T): BinaryResource;
}
export interface CloseableTube extends SerializedTube {
    close(): Promise<void>;
}
export declare type Resource = DirectoryResource | BinaryResource;
declare abstract class DirectoryResource {
    abstract list(): AsyncIterable<DirectoryResourceEntry>;
}
interface DirectoryResourceEntry {
    name: string;
    resource: Resource;
}
declare abstract class BinaryResource {
    abstract stream(): AsyncIterable<Uint8Array>;
}
export declare type OptionAddValueResult<Options> = {
    result: "success";
    updated: Partial<Options>;
} | {
    result: "invalid";
} | {
    result: "duplicate";
};
export declare abstract class OptionInfoBase {
    abstract readonly name: string;
    abstract readonly description: string;
}
export declare abstract class OptionInfoValue<Options> extends OptionInfoBase {
    abstract addOptionValue(prev: Partial<Options>, value: string): OptionAddValueResult<Options>;
}
export declare abstract class OptionInfoResource<Options> extends OptionInfoBase {
    abstract addOptionValue(prev: Partial<Options>, value: BinaryResource): OptionAddValueResult<Options>;
}
export declare type OptionInfo<Options> = OptionInfoValue<Options> | OptionInfoResource<Options>;
export declare type OptionsBuildResult<Options> = {
    success: true;
    options: Options;
} | {
    success: false;
    missing_value: string;
};
export interface OptionHandler<Options> {
    readonly options: readonly OptionInfo<Options>[];
    build(partial: Partial<Options>): OptionsBuildResult<Options>;
}
export interface OutputInfo<Output> {
    readonly name: string;
    readonly description: string;
    getValue(output: Output): Resource;
}
export interface OutputHandler<Output> {
    readonly options: readonly OptionInfo<Output>[];
}
export interface Backend<Options, Output> {
    emitModule(options: Options, tube: SerializedTube): Promise<Output>;
}
export interface TubeLoader<Options> {
    readonly supportedExtensions: readonly string[];
    load(options: Options, resource: BinaryResource): Promise<CloseableTube>;
}
export interface BuildOutputExecutor<Output> {
    execute(libraries: readonly {
        tubeName: readonly string[];
        output: Output;
    }[], buildOutput: Output): Promise<{
        exitCode: number;
        output: string;
    }>;
}
export interface PluginE {
    plugin<A>(f: <Options, Output>(plugin: Plugin<Options, Output>) => A): A;
}
export interface Plugin<Options, Output> extends PluginE {
    readonly optionHandler: OptionHandler<Options>;
    readonly outputHandler: OutputHandler<Output>;
    readonly backend: Promise<Backend<Options, Output>>;
    readonly tubeLoaders: Promise<readonly TubeLoader<Options>[]>;
    readonly buildOutputExecutors: Promise<BuildOutputExecutor<Output> | null>;
}
export {};
