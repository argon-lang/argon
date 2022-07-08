import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_SigTypeClass from "./SigTypeClass.js";
import * as sym_argon_tube_SigTypeFunction from "./SigTypeFunction.js";
import * as sym_argon_tube_SigTypeTrait from "./SigTypeTrait.js";
import * as sym_argon_tube_SigTypeTuple from "./SigTypeTuple.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export interface V1 {
    readonly erased: sym_unit.Unit;
    readonly class: sym_argon_tube_SigTypeClass.V1;
    readonly trait: sym_argon_tube_SigTypeTrait.V1;
    readonly tuple: sym_argon_tube_SigTypeTuple.V1;
    readonly function: sym_argon_tube_SigTypeFunction.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
