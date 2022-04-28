import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_MethodMember from "./MethodMember.js";
import * as sym_argon_tube_TraitOwner from "./TraitOwner.js";
import * as sym_argon_tube_TraitSignature from "./TraitSignature.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export interface V1 {
    readonly owner: sym_argon_tube_TraitOwner.V1<sym_unit.Unit>;
    readonly flags: sym_nat.Nat;
    readonly signature: sym_argon_tube_TraitSignature.V1;
    readonly methods: sym_list.List<sym_argon_tube_MethodMember.V1>;
    readonly staticMethods: sym_list.List<sym_argon_tube_MethodMember.V1>;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
