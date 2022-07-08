import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ExpressionConstructor from "./ExpressionConstructor.js";
import * as sym_list from "@verilization/runtime/list.js";
export interface V1 {
    readonly constructor: sym_argon_tube_ExpressionConstructor.V1;
    readonly args: sym_list.List<V1>;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
