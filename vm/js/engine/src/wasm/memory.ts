
export class WasmMemory {
    constructor(pages: number) {
        this.#buffer = new DataView(new ArrayBuffer(pages * 65536));
    }

    #buffer: DataView;

    storeI32(addr: number, value: number, align: number, offset: number): void {
        this.#buffer.setInt32(addr + offset, value, true);
    }

    storeBuffer(addr: number, data: Uint8Array): void {
        new Uint8Array(this.#buffer.buffer).set(data, addr);
    }

    loadI32(addr: number, align: number, offset: number): number {
        return this.#buffer.getInt32(addr + offset, true);
    }

    loadBuffer(addr: number, length: number): Uint8Array {
        return new Uint8Array(this.#buffer.buffer).slice(addr, addr + length);
    }
}
