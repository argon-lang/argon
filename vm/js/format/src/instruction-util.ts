import { Chunk } from "./chunk.js";

export interface InstructionContext {
    readonly chunk: Chunk;
    ip: number;
}

export function readIndex(ctx: InstructionContext): bigint {
    let result = 0n;
    let b;
    let shift = 0n;
    
    do {
        const item = ctx.chunk.bytecode[ctx.ip];
        if(item === undefined) {
            throw new Error("Unexpected end of data");
        }
        ++ctx.ip;

        b = item;

        result = (result << shift) | (BigInt(b & 0x7f));
        shift += 7n;
    } while((b & 0x80) != 0);

    return result;
}

export function readInt8(ctx: InstructionContext): number {
    const b = ctx.chunk.bytecode[ctx.ip];
    if(b === undefined) {
        throw new Error("Unexpected end of data");
    }
    ++ctx.ip;

    return b << 24 >> 24;
}
