import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_AccessModifier from "./AccessModifier.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1<TSig> {
    readonly id: sym_nat.Nat;
    readonly name: sym_option.Option<sym_argon_tube_Identifier.V1>;
    readonly accessModifier: sym_argon_tube_AccessModifier.V1;
    readonly sig: TSig;
}
export declare namespace V1 {
    function codec<TSig>(TSig_codec: Codec<TSig>): Codec<V1<TSig>>;
}
