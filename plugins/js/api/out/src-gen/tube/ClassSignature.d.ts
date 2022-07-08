import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ClassType from "./ClassType.js";
import * as sym_argon_tube_LocalVariableDeclaration from "./LocalVariableDeclaration.js";
import * as sym_argon_tube_Parameter from "./Parameter.js";
import * as sym_argon_tube_TraitType from "./TraitType.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
    readonly variables: sym_list.List<sym_argon_tube_LocalVariableDeclaration.V1>;
    readonly parameters: sym_list.List<sym_argon_tube_Parameter.V1>;
    readonly baseClass: sym_option.Option<sym_argon_tube_ClassType.V1>;
    readonly baseTraits: sym_list.List<sym_argon_tube_TraitType.V1>;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
