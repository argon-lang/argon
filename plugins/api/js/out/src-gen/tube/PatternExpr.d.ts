import { Codec } from "@verilization/runtime";
import * as sym_nat from "@verilization/runtime/nat.js";
export declare type V1 = {
    readonly tag: "binding";
    readonly binding: sym_nat.Nat;
} | {
    readonly tag: "castBinding";
    readonly castBinding: sym_nat.Nat;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
