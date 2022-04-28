import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_argon_tube_ModuleElementDeclaration from "./ModuleElementDeclaration.js";
export interface V1 {
    readonly trait: sym_argon_tube_ModuleElementDeclaration.V1<sym_argon_tube_ErasedSignatureParameterOnly.V1>;
    readonly class: sym_argon_tube_ModuleElementDeclaration.V1<sym_argon_tube_ErasedSignatureParameterOnly.V1>;
    readonly function: sym_argon_tube_ModuleElementDeclaration.V1<sym_argon_tube_ErasedSignatureParameterOnly.V1>;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
