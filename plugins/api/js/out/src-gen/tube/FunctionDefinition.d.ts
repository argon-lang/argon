import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_argon_tube_FunctionBody from "./FunctionBody.js";
import * as sym_argon_tube_FunctionOwner from "./FunctionOwner.js";
import * as sym_argon_tube_FunctionSignature from "./FunctionSignature.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export interface V1 {
    readonly owner: sym_argon_tube_FunctionOwner.V1<sym_unit.Unit>;
    readonly signature: sym_argon_tube_FunctionSignature.V1;
    readonly effects: sym_argon_tube_EffectInfo.V1;
    readonly body: sym_argon_tube_FunctionBody.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
