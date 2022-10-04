import { Chunk } from "@argon-lang/vm-format/lib/chunk.js";
import { getDefaultValue, TaggedValue, VMType } from "@argon-lang/vm-format/lib/vmtype.js";
import { NativeTrampoline } from "@argon-lang/vm-format/lib/vmfunction.js";


export interface StackFrame {
    native: false;
    readonly chunk: Chunk;
    ip: number;
    readonly stack: TaggedValue[];
    readonly variables: TaggedValue[];

    pop(): TaggedValue;
    push(value: TaggedValue): void;
}

export namespace StackFrame {
    export function create(chunk: Chunk, args?: readonly TaggedValue[]): StackFrame {
        const variables: TaggedValue[] = [...(args || [])];
        while(variables.length < chunk.variables.length) {
            variables.push(getDefaultValue(chunk.variables[variables.length]!));
        }

        return {
            native: false,
            chunk,
            ip: 0,
            stack: [],
            variables: variables,

            pop() {
                const value = this.stack.pop();
                if(value == undefined) {
                    throw new Error("Stack was empty");
                }
                return value;
            },

            push(value) {
                this.stack.push(value);
            }
        };
    }
}

export interface NativeStackFrame {
    native: true;
    resultType: VMType,
    continuation(arg: unknown): Promise<NativeTrampoline>;
}

export type CallStackFrame = StackFrame | NativeStackFrame;

