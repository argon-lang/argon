
import { arrayEqual, isEqual } from "./equal.js";
import { Memo } from "./memo.js";

export interface ObjectMap<T> {
    readonly [key: string]: T;
}

export interface MethodEntry {
    readonly symbol: Symbol;
    readonly implementation?: Function;
}

export interface TraitType {
    readonly symbol: Symbol;
    readonly methods: ObjectMap<MethodEntry>;
    readonly staticMethods: ObjectMap<Symbol>;
}


export interface ClassType {
    readonly prototype: {};
    readonly constructors: ObjectMap<Function>;
    readonly methods: ObjectMap<MethodEntry>;
    readonly staticMethods: ObjectMap<Symbol>;
    readonly fields: ObjectMap<Symbol>;
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


const traitTypeKeys: readonly (keyof TraitType)[] = ["symbol", "methods", "staticMethods"];
const classTypeKeys: readonly (keyof ClassType)[] = ["prototype", "constructors", "methods", "staticMethods", "fields"];

function createTypeFactory<TType>(keys: readonly (keyof TType)[]): (f: (...args: any[]) => TType) => (...args: any[]) => TType {
    return f => {
        const memo = new Memo(f);
        return (...args) => memo.get(...args);
    };
}


export const createTrait = createTypeFactory<TraitType>(traitTypeKeys);
export const createClass = createTypeFactory<ClassType>(classTypeKeys);


export function createObject(proto: object, ctor: (this: any, ...args: any[]) => void, ...args: any[]): any {
	const obj = Object.create(proto);
    ctor.call(obj, ...args);
    return obj;
}

