import type { BinToken } from "./bintoken.js";
import type { ESExpr } from "./esexpr.js";



function writeInt(bytes: number[], n: bigint): void {
    do {
        let b = Number(BigInt.asUintN(7, n));
        n >>= 7n;

        if(n > 0) {
            b |= 0x80;
        }

        bytes.push(b);
    } while(n > 0);
}


function writeToken(token: BinToken): Uint8Array {
    function writeWithInt(tag: number, n: bigint): Uint8Array {
        const bytes: number[] = [];

        let b = tag | Number(BigInt.asUintN(4, n));
        n >>= 4n;

        const isPos = n > 0;
        if(isPos) {
            b |= 0x10;
        }

        bytes.push(b);
        if(isPos) {
            writeInt(bytes, n);
        }

        return new Uint8Array(bytes);
    }

    function writeFixed(tag: number): Uint8Array {
        return new Uint8Array([ tag ]);
    }

    switch(token.type) {
        case "constructor": return writeWithInt(0x00, token.value);
        case "int": return writeWithInt(0x20, token.value);
        case "neg_int": return writeWithInt(0x40, token.value);
        case "string": return writeWithInt(0x60, token.value);
        case "binary": return writeWithInt(0x80, token.value);
        case "kwarg": return writeWithInt(0xA0, token.value);
        case "null": return writeFixed(0xE0);
        case "constructor_end": return writeFixed(0xE1);
        case "true": return writeFixed(0xE2);
        case "false": return writeFixed(0xE3);
        case "float32": return writeFixed(0xE4);
        case "float64": return writeFixed(0xE5);
    }
}

function asIterable<T>(iterator: AsyncIterator<T>): AsyncIterable<T> {
    return {
        [Symbol.asyncIterator](): AsyncIterator<T> {
            return iterator;
        }
    }
}

export class ESExprBinaryWriter {
    constructor(symbolTable: readonly string[]) {
        this.#symbolTable = symbolTable;
    }

    readonly #symbolTable: readonly string[];

    async *write(expr: ESExpr): AsyncIterator<Uint8Array> {
        switch(typeof(expr)) {
            case "object":
                if(expr === null) {
                    yield writeToken({ type: "null" });
                }
                else if(expr instanceof Uint8Array) {
                    yield writeToken({ type: "binary", value: BigInt(expr.length) });
                    yield expr;
                }
                else if(expr.type === "float32") {
                    yield writeToken({ type: "float32" });
                    
                    const buff = new ArrayBuffer(4);
                    (new Float32Array(buff))[0] = expr.value;
                    const bits = (new Uint32Array(buff))[0]!;
                    const bytes = new Uint8Array(buff);
                    
                    for(let i = 0; i < 4; ++i) {
                        bytes[i] = (bits >>> (i * 8)) & 0xFF;
                    }

                    yield bytes;
                }
                else {
                    const index = this.#getSymbolIndex(expr.constructor);
                    yield writeToken({ type: "constructor", value: index });
                    for(const [k, v] of expr.kwargs) {
                        const index2 = this.#getSymbolIndex(expr.constructor);
                        yield writeToken({ type: "kwarg", value: index2 });
                        yield* asIterable(this.write(v));
                    }
                    for(const arg of expr.args) {
                        yield* asIterable(this.write(arg));
                    }
                    yield writeToken({ type: "constructor_end" });
                }
                break;

            case "boolean":
                if(expr) {
                    yield writeToken({ type: "true" });
                }
                else {
                    yield writeToken({ type: "false" });
                }
                break;
            
            case "bigint":
                if(expr >= 0) {
                    yield writeToken({ type: "int", value: expr });
                }
                else {
                    yield writeToken({ type: "neg_int", value: (-expr) - 1n });
                }
                break;

            case "string":
                yield writeToken({ type: "string", value: BigInt(expr.length) });
                yield new TextEncoder().encode(expr);
                break;

            case "number":
            {
                yield writeToken({ type: "float64" });
                const buff = new ArrayBuffer(8);
                (new Float64Array(buff))[0] = expr;
                const bits = (new BigUint64Array(buff))[0]!;
                const bytes = new Uint8Array(buff);
                
                for(let i = 0; i < 8; ++i) {
                    bytes[i] = Number(BigInt.asUintN(8, bits >> (BigInt(i) * 8n)));
                }

                yield bytes;

                break;
            }

        }
    }

    #getSymbolIndex(sym: string): bigint {
        const index = this.#symbolTable.indexOf(sym);
        if(index < 0) {
            throw new Error("Found symbol not in symbol table");
        }

        return BigInt(index);
    }

    

}
