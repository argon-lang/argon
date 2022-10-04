import { Chunk } from "./chunk.js";
import { VMType } from "./vmtype.js";


interface VMFunctionCommon {
    readonly parameterTypes: readonly VMType[];
    readonly returnType: VMType;
}

export interface BytecodeVMFunction extends VMFunctionCommon {
    native: false;
    chunk: Chunk;
}


export type NativeTrampoline =
    {
        type: "result";
        readonly value: unknown;
    } |
    {
        type: "delay-function";
        readonly next: VMFunction;
        readonly args: readonly unknown[];
        continuation(arg: unknown): Promise<NativeTrampoline>;
    } |
    {
        type: "delay";
        invoke(): Promise<NativeTrampoline>;
    };


interface NativeFunction extends VMFunctionCommon {
    native: true;
    invoke(...args: unknown[]): Promise<NativeTrampoline>;
};

export type VMFunction = BytecodeVMFunction | NativeFunction;


export type NativeFunctions = {
    [name: string]: NativeFunction;
};

