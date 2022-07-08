import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ExpressionWithVariables from "./ExpressionWithVariables.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export declare type V1 = {
    readonly tag: "expressionBody";
    readonly expressionBody: sym_argon_tube_ExpressionWithVariables.V1;
} | {
    readonly tag: "externalImplementation";
    readonly externalImplementation: sym_unit.Unit;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
