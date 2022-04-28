import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_int from "@verilization/runtime/int.js";
export interface V1 {
    readonly ownerClass: sym_int.Int;
    readonly signature: sym_argon_tube_ErasedSignatureParameterOnly.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
