import { Codec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export declare type V1 = {
    readonly tag: "mutable";
    readonly mutable: sym_unit.Unit;
} | {
    readonly tag: "nonMutable";
    readonly nonMutable: sym_unit.Unit;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
