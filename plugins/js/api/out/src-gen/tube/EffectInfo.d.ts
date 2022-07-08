import { Codec } from "@verilization/runtime";
import * as sym_bool from "@verilization/runtime/bool.js";
export declare type V1 = {
    readonly tag: "isPure";
    readonly isPure: sym_bool.Bool;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
