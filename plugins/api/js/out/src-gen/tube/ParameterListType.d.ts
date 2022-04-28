import { Codec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export declare type V1 = {
    readonly tag: "normalList";
    readonly normalList: sym_unit.Unit;
} | {
    readonly tag: "inferrableList";
    readonly inferrableList: sym_unit.Unit;
} | {
    readonly tag: "inferrableList2";
    readonly inferrableList2: sym_unit.Unit;
} | {
    readonly tag: "requiresList";
    readonly requiresList: sym_unit.Unit;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
