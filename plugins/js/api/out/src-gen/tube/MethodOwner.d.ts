import { Codec } from "@verilization/runtime";
import * as sym_int from "@verilization/runtime/int.js";
export declare type V1 = {
    readonly tag: "byClass";
    readonly byClass: sym_int.Int;
} | {
    readonly tag: "byClassStatic";
    readonly byClassStatic: sym_int.Int;
} | {
    readonly tag: "byTrait";
    readonly byTrait: sym_int.Int;
} | {
    readonly tag: "byTraitStatic";
    readonly byTraitStatic: sym_int.Int;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
