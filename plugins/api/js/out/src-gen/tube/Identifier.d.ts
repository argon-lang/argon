import { Codec } from "@verilization/runtime";
import * as sym_string from "@verilization/runtime/string.js";
export declare type V1 = {
    readonly tag: "normalName";
    readonly normalName: sym_string.String;
} | {
    readonly tag: "operator";
    readonly operator: sym_string.String;
} | {
    readonly tag: "extension";
    readonly extension: V1;
} | {
    readonly tag: "inverse";
    readonly inverse: V1;
} | {
    readonly tag: "update";
    readonly update: V1;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
