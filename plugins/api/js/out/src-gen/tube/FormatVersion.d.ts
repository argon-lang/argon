import { Codec } from "@verilization/runtime";
import * as sym_nat from "@verilization/runtime/nat.js";
export interface V1 {
    readonly major: sym_nat.Nat;
    readonly minor: sym_nat.Nat;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
