import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_argon_tube_FunctionBody from "./FunctionBody.js";
import * as sym_argon_tube_FunctionSignature from "./FunctionSignature.js";
import * as sym_argon_tube_MethodOwner from "./MethodOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
    readonly owner: sym_argon_tube_MethodOwner.V1;
    readonly flags: sym_nat.Nat;
    readonly signature: sym_argon_tube_FunctionSignature.V1;
    readonly effects: sym_argon_tube_EffectInfo.V1;
    readonly body: sym_option.Option<sym_argon_tube_FunctionBody.V1>;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
