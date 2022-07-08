import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_ByModuleOwner from "./ByModuleOwner.js";
export declare type V1<Tube> = {
    readonly tag: "byModule";
    readonly byModule: sym_argon_tube_ByModuleOwner.V1<Tube>;
};
export declare namespace V1 {
    function codec<Tube>(Tube_codec: Codec<Tube>): Codec<V1<Tube>>;
}
