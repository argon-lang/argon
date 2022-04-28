import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ClassField from "./ClassField.js";
import * as sym_argon_tube_ClassOwner from "./ClassOwner.js";
import * as sym_argon_tube_ClassSignature from "./ClassSignature.js";
import * as sym_argon_tube_ConstructorMember from "./ConstructorMember.js";
import * as sym_argon_tube_MethodMember from "./MethodMember.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export interface V1 {
    readonly owner: sym_argon_tube_ClassOwner.V1<sym_unit.Unit>;
    readonly flags: sym_nat.Nat;
    readonly signature: sym_argon_tube_ClassSignature.V1;
    readonly fields: sym_list.List<sym_argon_tube_ClassField.V1>;
    readonly methods: sym_list.List<sym_argon_tube_MethodMember.V1>;
    readonly staticMethods: sym_list.List<sym_argon_tube_MethodMember.V1>;
    readonly constructors: sym_list.List<sym_argon_tube_ConstructorMember.V1>;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
