
export type VMType =
    { type: "int8" } |
    { type: "int16" } |
    { type: "int32" } |
    { type: "int64" } |
    { type: "float32" } |
    { type: "float64" } |
    { type: "object_reference" } |
    { type: "tuple"; elements: readonly VMType[]; };

export namespace VMType {
    export const Int8: VMType = { type: "int8" };
    export const Int16: VMType = { type: "int16" };
    export const Int32: VMType = { type: "int32" };
    export const Int64: VMType = { type: "int64" };
    export const Float32: VMType = { type: "float32" };
    export const Float64: VMType = { type: "float64" };
    export const ObjectReference: VMType = { type: "object_reference" };
    export function tuple(...elements: readonly VMType[]): VMType {
        return {
            type: "tuple",
            elements,
        }
    }
}

export interface TypeMapping {
    int8: number;
    int16: number;
    int32: number;
    int64: bigint;
    float32: number;
    float64: number;
    tuple: readonly unknown[];
    object_reference: unknown;
}

export interface TaggedValue {
    readonly type: VMType;
    readonly value: unknown;
}

export function getDefaultValue(t: VMType): TaggedValue {
    switch(t.type) {
        case "int8":
        case "int16":
        case "int32":
        case "int64":
        case "float32":
        case "float64":
            return { type: t, value: 0 };

        case "object_reference":
            return { type: t, value: null };

        case "tuple":
            return { type: t, value: t.elements.map(getDefaultValue) };
    }
}

export function isSameType(a: VMType, b: VMType): boolean {
    if(a.type !== b.type) {
        return false;
    }

    if(a.type == "tuple" && b.type == "tuple") {
        if(a.elements.length !== b.elements.length) {
            return false;
        }

        for(let i = 0; i < a.elements.length; ++i) {
            if(!isSameType(a.elements[i]!, b.elements[i]!)) {
                return false;
            }
        }
    }

    return true;
}

