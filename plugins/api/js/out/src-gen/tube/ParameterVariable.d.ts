import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ParameterVariableOwner from "./ParameterVariableOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
export interface V1 {
    readonly owner: sym_argon_tube_ParameterVariableOwner.V1;
    readonly index: sym_nat.Nat;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
