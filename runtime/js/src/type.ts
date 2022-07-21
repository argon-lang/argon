
import { arrayEqual, isEqual } from "./equal.js";
import { Memo } from "./memo.js";
import { SymbolMap } from "./symbol-map.js";

export interface TraitType {
    readonly symbol: Symbol;
    readonly baseTraits: readonly TraitType[];
    readonly methods: SymbolMap;
    readonly fields: SymbolMap;
}


export interface ClassType {
    readonly constructor: Function;
    readonly baseClass: ClassType | null;
    readonly baseTraits: readonly TraitType[];
    readonly methods: SymbolMap;
    readonly fields: SymbolMap;
}

export class FunctionType {
    constructor(public readonly argument: Type, public readonly result: Type) {}

    canEqual(other: unknown): boolean {
        return other instanceof FunctionType;
    }

    equals(other: unknown): boolean {
        if(!(other instanceof FunctionType)) {
            return false;
        }

        return isEqual(this.argument, other.argument) && isEqual(this.result, other.result);
    }
}

export class TupleType {
    constructor(elements: readonly Type[]) {
        this.elements = Object.freeze([...elements]);
    }

    elements: readonly Type[];


    canEqual(other: unknown): boolean {
        return other instanceof TupleType;
    }

    equals(other: unknown): boolean {
        if(!(other instanceof TupleType)) {
            return false;
        }

        return arrayEqual(this.elements, other.elements);
    }
}



type Type = TraitType | ClassType | FunctionType | TupleType;


const traitTypeKeys: readonly (keyof TraitType)[] = ["symbol", "baseTraits", "methods", "fields"];
const classTypeKeys: readonly (keyof ClassType)[] = ["constructor", "baseClass", "baseTraits", "methods", "fields"];

function createTypeFactory<TType>(keys: readonly (keyof TType)[]): (f: (...args: any[]) => TType) => (...args: any[]) => TType {
    return f => {
        const memo = new Memo(f);
        return (...args) => memo.get(...args);
    };
}


export const createTrait = createTypeFactory<TraitType>(traitTypeKeys);
export const createClass = createTypeFactory<ClassType>(classTypeKeys);
