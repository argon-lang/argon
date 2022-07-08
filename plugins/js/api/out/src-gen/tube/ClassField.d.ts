import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ExpressionWithVariables from "./ExpressionWithVariables.js";
import * as sym_argon_tube_Mutability from "./Mutability.js";
import * as sym_string from "@verilization/runtime/string.js";
export interface V1 {
    readonly mutability: sym_argon_tube_Mutability.V1;
    readonly name: sym_string.String;
    readonly fieldType: sym_argon_tube_ExpressionWithVariables.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
