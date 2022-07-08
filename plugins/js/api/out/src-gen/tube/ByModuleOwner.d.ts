import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ModulePath from "./ModulePath.js";
export interface V1<Tube> {
    readonly tube: Tube;
    readonly path: sym_argon_tube_ModulePath.V1;
}
export declare namespace V1 {
    function codec<Tube>(Tube_codec: Codec<Tube>): Codec<V1<Tube>>;
}
