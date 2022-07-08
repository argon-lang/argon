import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ClassConstructorBody from "./ClassConstructorBody.js";
import * as sym_argon_tube_ClassConstructorSignature from "./ClassConstructorSignature.js";
import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_int from "@verilization/runtime/int.js";
export interface V1 {
    readonly ownerClass: sym_int.Int;
    readonly signature: sym_argon_tube_ClassConstructorSignature.V1;
    readonly effects: sym_argon_tube_EffectInfo.V1;
    readonly body: sym_argon_tube_ClassConstructorBody.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
