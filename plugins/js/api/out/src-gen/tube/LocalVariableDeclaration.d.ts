import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_bool from "@verilization/runtime/bool.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
    readonly varType: sym_argon_tube_Expression.V1;
    readonly name: sym_option.Option<sym_argon_tube_Identifier.V1>;
    readonly isMutable: sym_bool.Bool;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
