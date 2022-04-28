import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ParameterVariable from "./ParameterVariable.js";
import * as sym_nat from "@verilization/runtime/nat.js";
export declare type V1 = {
    readonly tag: "localVariable";
    readonly localVariable: sym_nat.Nat;
} | {
    readonly tag: "instanceVariable";
    readonly instanceVariable: sym_nat.Nat;
} | {
    readonly tag: "parameter";
    readonly parameter: sym_argon_tube_ParameterVariable.V1;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
