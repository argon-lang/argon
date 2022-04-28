import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_ParameterListType from "./ParameterListType.js";
import * as sym_bool from "@verilization/runtime/bool.js";
export interface V1 {
    readonly listType: sym_argon_tube_ParameterListType.V1;
    readonly isErased: sym_bool.Bool;
    readonly type: sym_argon_tube_Expression.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
