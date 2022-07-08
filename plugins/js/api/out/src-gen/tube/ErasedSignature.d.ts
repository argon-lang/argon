import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_SigType from "./SigType.js";
import * as sym_list from "@verilization/runtime/list.js";
export interface V1 {
    readonly parameterTypes: sym_list.List<sym_argon_tube_SigType.V1>;
    readonly resultType: sym_argon_tube_SigType.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
