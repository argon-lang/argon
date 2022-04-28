import { Codec } from "@verilization/runtime";
import * as sym_nat from "@verilization/runtime/nat.js";
export declare type V1 = {
    readonly tag: "byMethod";
    readonly byMethod: sym_nat.Nat;
} | {
    readonly tag: "byFunction";
    readonly byFunction: sym_nat.Nat;
} | {
    readonly tag: "byClass";
    readonly byClass: sym_nat.Nat;
} | {
    readonly tag: "byTrait";
    readonly byTrait: sym_nat.Nat;
} | {
    readonly tag: "byClassConstructor";
    readonly byClassConstructor: sym_nat.Nat;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
