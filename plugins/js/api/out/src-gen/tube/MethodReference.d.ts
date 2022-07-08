import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ErasedSignature from "./ErasedSignature.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_argon_tube_MethodOwner from "./MethodOwner.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
    readonly owner: sym_argon_tube_MethodOwner.V1;
    readonly name: sym_option.Option<sym_argon_tube_Identifier.V1>;
    readonly signature: sym_argon_tube_ErasedSignature.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
