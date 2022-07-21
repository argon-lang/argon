import { arrayEqual } from "./equal.js";

export class Memo<K extends any[], V> {
    constructor(f: (...args: K) => V) {
        this.#f = f;
        this.#entries = [];
    }

    #f: (...args: K) => V;
    #entries: [K, V][];

    get(...args: K): V {
        for(const [k, v] of this.#entries) {
            if(arrayEqual(k, args)) {
                return v;
            }
        }

        const value = this.#f(...args);
        this.#entries.push([[...args] as K, value]);
        return value;
    }

}
