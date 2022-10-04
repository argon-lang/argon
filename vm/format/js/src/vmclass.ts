import { VMType } from "./vmtype.js";

export interface SlotInfo {
    readonly parameterTypes: readonly VMType[];
    readonly returnType: VMType;
    readonly symbol: symbol;
}

export interface SlotKey {
    classId: bigint;
    index: bigint;
}

export interface FieldInfo {
    readonly fieldType: VMType;
    readonly symbol: symbol;
}

export interface VMClass {
    readonly prototypeObject: object;

    readonly baseClass: VMClass | null;
    readonly fields: readonly FieldInfo[];
    readonly slots: readonly SlotInfo[];
}
