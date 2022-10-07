import { NativeFunctions } from "@argon-lang/vm-format/lib/vmfunction.js";
import { VMType } from "@argon-lang/vm-format/lib/vmtype.js";
import { WasmMemory } from "./wasm/memory.js";

export class StandardLibrary {

    constructor() {}

    print(value: string): void {
        console.log(value);
    }

    int32_to_string(value: number): string {
        return value.toString();
    }

    uint32_to_string(value: number): string {
        return (value >>> 0).toString();
    }

    float64_to_string(value: number): string {
        return value.toString();
    }

    #mainMemory: WasmMemory | null = null;

    wasm_memory_new(pages: number): WasmMemory {
        const memory = new WasmMemory(pages >>> 0);
        if(this.#mainMemory === null) {
            this.#mainMemory = memory;
        }
        return memory;
    }

    wasm_memory_store_i32(addr: number, value: number, align: number, offset: number, memory: WasmMemory): void {
        memory.storeI32(addr >>> 0, value, align & 0xFF, offset >>> 0);
    }

    wasm_memory_store_buffer(memory: WasmMemory, addr: number, data: Uint8Array): void {
        memory.storeBuffer(addr >>> 0, data);
    }

    wasi_fd_write(fd: number, iovs: number, iovs_len: number, nwritten: number): number {
        const stream = (() => {
            switch(fd) {
                case 1:
                    return process.stdout;

                default:
                    throw new Error("Unknown fd");
            }
        })();

        const memory = this.#mainMemory;
        if(memory === null) {
            throw new Error("Main memory is not set");
        }

        iovs = iovs >>> 0;
        iovs_len = iovs_len >>> 0;
        nwritten = nwritten >>> 0;

        let bytesWritten = 0;
        for(let i = 0; i < iovs_len; ++i) {
            const iov_base = memory.loadI32(iovs, 0, i * 8) >>> 0;
            const iov_len = memory.loadI32(iovs, 0, i * 8 + 4);
            const data = memory.loadBuffer(iov_base, iov_len);
            stream.write(data);
            bytesWritten += iov_len;
        }
        memory.storeI32(nwritten, bytesWritten, 0, 0);
        return bytesWritten;
    }

    nativeFunctions: NativeFunctions = (() => {
        const stdlib = this;
        return {
            print: {
                native: true,
                parameterTypes: [ VMType.ObjectReference ],
                returnType: VMType.tuple(),
                async invoke(...args) {
                    stdlib.print(args[0] as string);
                    return { type: "result", value: [] };
                }
            },

            int32_to_string: {
                native: true,
                parameterTypes: [ VMType.Int32 ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    const result = stdlib.int32_to_string(args[0] as number);
                    return { type: "result", value: result };
                }
            },

            uint32_to_string: {
                native: true,
                parameterTypes: [ VMType.Int32 ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    const result = stdlib.uint32_to_string(args[0] as number);
                    return { type: "result", value: result };
                }
            },

            float64_to_string: {
                native: true,
                parameterTypes: [ VMType.Int32 ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    const result = stdlib.float64_to_string(args[0] as number);
                    return { type: "result", value: result };
                }
            },

            wasm_memory_new: {
                native: true,
                parameterTypes: [ VMType.Int32 ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    const result = stdlib.wasm_memory_new(args[0] as number);
                    return { type: "result", value: result };
                }
            },

            wasm_memory_store_i32: {
                native: true,
                parameterTypes: [ VMType.Int32, VMType.Int32, VMType.Int32, VMType.Int32, VMType.ObjectReference ],
                returnType: VMType.tuple(),
                async invoke(...args) {
                    stdlib.wasm_memory_store_i32(args[0] as number, args[1] as number, args[2] as number, args[3] as number, args[4] as WasmMemory);
                    return { type: "result", value: [] };
                }
            },

            wasm_memory_store_buffer: {
                native: true,
                parameterTypes: [ VMType.ObjectReference, VMType.Int32, VMType.ObjectReference ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    stdlib.wasm_memory_store_buffer(args[0] as WasmMemory, args[1] as number, args[2] as Uint8Array);
                    return { type: "result", value: [] };
                }
            },

            "wasi_snapshot_preview1::fd_write": {
                native: true,
                parameterTypes: [ VMType.Int32, VMType.Int32, VMType.Int32, VMType.Int32 ],
                returnType: VMType.Int32,
                async invoke(...args) {
                    const result = stdlib.wasi_fd_write(args[0] as number, args[1] as number, args[2] as number, args[3] as number);
                    return { type: "result", value: result };
                }
            },
        };
    })();
}
