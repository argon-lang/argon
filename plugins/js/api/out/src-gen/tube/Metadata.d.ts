import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_TubeReference from "./TubeReference.js";
import * as sym_argon_tube_TubeType from "./TubeType.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_string from "@verilization/runtime/string.js";
export interface V1 {
    readonly name: sym_string.String;
    readonly moduleType: sym_argon_tube_TubeType.V1;
    readonly platforms: sym_list.List<sym_string.String>;
    readonly references: sym_list.List<sym_argon_tube_TubeReference.V1>;
    readonly moduleCount: sym_nat.Nat;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
