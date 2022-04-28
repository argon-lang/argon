import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_argon_tube_TraitOwner from "./TraitOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
    readonly owner: sym_argon_tube_TraitOwner.V1<sym_nat.Nat>;
    readonly name: sym_option.Option<sym_argon_tube_Identifier.V1>;
    readonly signature: sym_argon_tube_ErasedSignatureParameterOnly.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
