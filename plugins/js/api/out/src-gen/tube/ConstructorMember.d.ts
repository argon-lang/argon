import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_AccessModifier from "./AccessModifier.js";
import * as sym_int from "@verilization/runtime/int.js";
export interface V1 {
    readonly id: sym_int.Int;
    readonly accessModifier: sym_argon_tube_AccessModifier.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
