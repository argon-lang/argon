import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_FieldInitializer from "./FieldInitializer.js";
export declare type V1 = {
    readonly tag: "expression";
    readonly expression: sym_argon_tube_Expression.V1;
} | {
    readonly tag: "fieldInitializer";
    readonly fieldInitializer: sym_argon_tube_FieldInitializer.V1;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
