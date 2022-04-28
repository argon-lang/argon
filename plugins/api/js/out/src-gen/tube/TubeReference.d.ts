import { Codec } from "@verilization/runtime";
import * as sym_string from "@verilization/runtime/string.js";
export interface V1 {
    readonly name: sym_string.String;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
