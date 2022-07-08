import { Codec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export declare type V1 = {
    readonly tag: "interface";
    readonly interface: sym_unit.Unit;
} | {
    readonly tag: "definition";
    readonly definition: sym_unit.Unit;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
