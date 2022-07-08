import { Codec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export declare type V1 = {
    readonly tag: "public";
    readonly public: sym_unit.Unit;
} | {
    readonly tag: "tubePrivate";
    readonly tubePrivate: sym_unit.Unit;
} | {
    readonly tag: "filePrivate";
    readonly filePrivate: sym_unit.Unit;
} | {
    readonly tag: "tubeOrProtected";
    readonly tubeOrProtected: sym_unit.Unit;
} | {
    readonly tag: "tubeAndProtected";
    readonly tubeAndProtected: sym_unit.Unit;
} | {
    readonly tag: "protected";
    readonly protected: sym_unit.Unit;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
