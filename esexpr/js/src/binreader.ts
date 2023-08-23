import type { BinToken } from "./bintoken.js";
import type { ESExpr } from "./esexpr.js";


type ESExprPlus =
    | ESExpr
    | { type: "constructor_end" }
    | { type: "keyword_arg", readonly keyword: string, readonly value: ESExpr };


export class ESExprBinaryReader {
    constructor(symbolTable: readonly string[], iter: AsyncIterator<Uint8Array>) {
        this.#symbolTable = symbolTable;
        this.#iter = iter;
    }

    readonly #symbolTable: readonly string[];
    readonly #iter: AsyncIterator<Uint8Array>;
    #current: Uint8Array | null = null;
    #currentIndex: number = 0;

    async #ensureAvailable(): Promise<Uint8Array | null> {
        if(this.#current === null) {
            const data = await this.#iter.next()
            if(data.done) {
                return null;
            }

            this.#current = data.value;
            this.#currentIndex = 0;
        }

        return this.#current;
    }

    async #next(): Promise<number | null> {
        const res = await this.#peekNext();
        if(res === null) {
            return null;
        }

        ++this.#currentIndex;
        if(this.#current !== null && this.#currentIndex >= this.#current.length) {
            this.#current = null;
            this.#currentIndex = 0;
        }

        return res;
    }

    async #peekNext(): Promise<number | null> {
        const current = await this.#ensureAvailable();
        if(current === null) {
            return null;
        }
        return current[this.#currentIndex]!;
    }

    async read(): Promise<ESExpr | undefined> {
        if(await this.#peekNext() === null) {
            return undefined;
        }

        return await this.#readExpr();
    }

    async *readAll(): AsyncIterator<ESExpr> {
        while(await this.#peekNext() !== null) {
            yield await this.#readExpr();
        }
    }


    async #nextToken(): Promise<BinToken> {
        const b = await this.#next();
        if(b === null) {
            throw new Error("Unexpected EOF");
        }

        const t = (() => {
            switch(b & 0xE0) {
                case 0x00: return "constructor";
                case 0x20: return "int";
                case 0x40: return "neg_int";
                case 0x60: return "string";
                case 0x80: return "binary";
                case 0xA0: return "kwarg";
                case 0xC0: throw new Error("Use of reserved tag");
                default: return null;
            }
        })();

        if(t === null) {
            const t2 = (() => {
                switch(b) {
                    case 0xE0: return "null";
                    case 0xE1: return "constructor_end";
                    case 0xE2: return "true";
                    case 0xE3: return "false";
                    case 0xE4: return "float32";
                    case 0xE5: return "float64";
                    default: throw new Error("Use of reserved tag");
                }
            })();

            return { type: t2 };
        }
        else {
            let i = BigInt(b & 0x0F);
            if((b & 0x10) == 0x10) {
                i = await this.#readIntAcc(i, 4n);
            }

            return { type: t, value: i };
        }
    }

    async #readInt(): Promise<bigint> {
        return await this.#readIntAcc(0n, 0n);
    }

    async #readIntAcc(acc: bigint, bits: bigint): Promise<bigint> {
        while(true) {
            const b = await this.#next();
            if(b === null) {
                throw new Error("Unexpected EOF");
            }

            acc |= BigInt(b & 0x7F) << bits;
            bits += 7n;

            if((b & 0x80) == 0x00) {
                return acc;
            }
        }
    }


    async #readExpr(): Promise<ESExpr> {
        const expr = await this.#readExprPlus();
        if(typeof(expr) === "object" && expr !== null && "type" in expr && (expr.type === "constructor_end" || expr.type === "keyword_arg")) {
            throw new Error("Found unexpected constructor end");
        }
        return expr;
    }

    async #readExprPlus(): Promise<ESExprPlus> {
        const token = await this.#nextToken();
        switch(token.type) {
            case "constructor":
            {
                const sym = this.#symbolTable[Number(token.value)];
                if(sym === undefined) {
                    throw new Error("Invalid symbol index");
                }

                const kwargs = new Map<string, ESExpr>();
                const args: ESExpr[] = [];
                while(true) {
                    const arg = await this.#readExprPlus();
                    if(typeof(arg) === "object" && arg !== null && "type" in arg) {
                        if(arg.type === "constructor_end") {
                            break;
                        }
                        else if(arg.type === "keyword_arg") {
                            kwargs.set(arg.keyword, arg.value);
                            continue;
                        }
                    }

                    args.push(arg);
                }

                return { type: "constructed", constructor: sym, kwargs, args };
            }

            case "int":
                return token.value;

            case "neg_int":
                return -(token.value + 1n);

            case "string":
                return new TextDecoder().decode(await this.#readByteString(token.value));

            case "binary":
                return await this.#readByteString(token.value);

            case "kwarg":
            {
                const sym = this.#symbolTable[Number(token.value)];
                if(sym === undefined) {
                    throw new Error("Invalid symbol index");
                }

                const value = await this.#readExpr();

                return { type: "keyword_arg", keyword: sym, value };
            }

            case "null":
                return null;

            case "constructor_end":
                return { type: "constructor_end" };

            case "true":
                return true;

            case "false":
                return false;

            case "float32":
            {
                let bits = 0;
                for(let i = 0; i < 4; ++i) {
                    const b = await this.#next();
                    if(b === null) {
                        throw new Error("Unexpected EOF");
                    }

                    bits |= (b & 0xFF) << (i * 8);
                }

                const buff = new ArrayBuffer(4);
                (new Uint32Array(buff))[0] = bits;
                return {
                    type: "float32",
                    value: (new Float32Array(buff))[0]!,
                };
            }

            case "float64":
            {
                let bits = 0n;
                for(let i = 0n; i < 8n; ++i) {
                    const b = await this.#next();
                    if(b === null) {
                        throw new Error("Unexpected EOF");
                    }

                    bits |= BigInt((b & 0xFF)) << (i * 8n);
                }

                const buff = new ArrayBuffer(8);
                (new BigUint64Array(buff))[0] = bits;
                return (new Float64Array(buff))[0]!;
            }
        }
    }


    async #readByteString(len: bigint): Promise<Uint8Array> {
        const b = new Uint8Array(Number(len));
        for(let i = 0; i < b.length;) {
            const current = await this.#ensureAvailable();
            if(current == null) {
                throw new Error("Unexpected EOF");
            }

            const limit = Math.min(current.length - this.#currentIndex, b.length - i);

            b.set(current.slice(this.#currentIndex, this.#currentIndex + limit), i);

            this.#currentIndex += limit;
            i += limit;

            if(this.#currentIndex >= current.length) {
                this.#current = null;
                this.#currentIndex = 0;
            }
        }
        return b;
    }


}
