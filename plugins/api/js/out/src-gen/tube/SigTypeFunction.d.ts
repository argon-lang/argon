import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_SigType from "./SigType.js";
export interface V1 {
    readonly argumentType: sym_argon_tube_SigType.V1;
    readonly resultType: sym_argon_tube_SigType.V1;
}
export declare namespace V1 {
    const codec: Codec<V1>;
}
