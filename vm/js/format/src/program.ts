import { VMClass, SlotInfo } from "./vmclass.js";
import { BytecodeVMFunction, NativeFunctions, VMFunction } from "./vmfunction.js";
import * as proto from "./proto/argonvm.js";
import { Chunk } from "./chunk.js";
import { TaggedValue, VMType } from "./vmtype.js";

export interface Program {
    getFunction(index: bigint): VMFunction;
    getClass(index: bigint): VMClass;
    getClassIndex(classInstance: VMClass): bigint;
}

export function loadProgram(program: proto.Program, nativeFunctions: NativeFunctions): [Program, Chunk] {
    const entrypoint = loadChunk(program.entrypoint!);
    const loadedProgram = new LoadedProgram(program, nativeFunctions);
    return [loadedProgram, entrypoint];
}

function loadChunk(chunk: proto.Chunk): Chunk {
    return {
        constants: chunk.constants.map(loadConstant),
        variables: chunk.variableTypes.map(loadVMType),
        labelTargets: [...chunk.labelTargets],
        bytecode: chunk.bytecode,
    };
}

function loadConstant(value: proto.ConstantValue): TaggedValue {
    switch(value.value.oneofKind) {
        case "int8":
            return {
                type: VMType.Int8,
                value: value.value.int8,
            };

        case "int16":
            return {
                type: VMType.Int16,
                value: value.value.int16,
            };

        case "int32":
            return {
                type: VMType.Int32,
                value: value.value.int32,
            };

        case "int64":
            return {
                type: VMType.Int8,
                value: value.value.int64,
            };

        case "float32":
            return {
                type: VMType.Float32,
                value: value.value.float32,
            };

        case "float64":
            return {
                type: VMType.Float64,
                value: value.value.float64,
            };

        case "stringLiteral":
            return {
                type: VMType.ObjectReference,
                value: value.value.stringLiteral,
            };

        case "tuple":
        {
            const elements = value.value.tuple.elements.map(loadConstant);
            return {
                type: VMType.tuple(...elements.map(element => element.type)),
                value: elements.map(element => element.value),
            };
        }

        case undefined:
            throw new Error("Unknown constant value type");
    }
}

function loadVMType(t: proto.ValueType): VMType {
    switch(t.type.oneofKind) {
        case "simple":
            return ((): VMType => {
                switch(t.type.simple) {
                    case proto.ValueTypeSimple.UNSPECIFIED$: throw new Error("Invalid type");
                    case proto.ValueTypeSimple.INT8: return VMType.Int8;
                    case proto.ValueTypeSimple.INT16: return VMType.Int16;
                    case proto.ValueTypeSimple.INT32: return VMType.Int32;
                    case proto.ValueTypeSimple.INT64: return VMType.Int64;
                    case proto.ValueTypeSimple.FLOAT32: return VMType.Float32;
                    case proto.ValueTypeSimple.FLOAT64: return VMType.Float64;
                    case proto.ValueTypeSimple.OBJECT_REFERENCE: return VMType.ObjectReference;
                }
            })();

        case "tuple":
            return VMType.tuple(...t.type.tuple.elements.map(loadVMType));

        case undefined:
            throw new Error("Invalid type");
    }
}

class LoadedProgram implements Program {
    constructor(program: proto.Program, nativeFunctions: NativeFunctions) {
        this.#program = program;
        this.#nativeFunctions = nativeFunctions;
    }

    readonly #program: proto.Program;
    readonly #nativeFunctions: NativeFunctions;
    readonly #functions: VMFunction[] = [];
    readonly #classes: VMClass[] = [];


    getFunction(index: bigint): VMFunction {
        const index2 = Number(index);
        let func = this.#functions[index2];
        if(func === undefined) {
            const protoFunc = this.#program.functions[index2];
            if(protoFunc === undefined) {
                throw new Error("Invalid function");
            }

            func = loadFunction(protoFunc, this.#nativeFunctions);
            this.#functions[index2] = func;
        }
        return func;
    }

    getClass(index: bigint): VMClass {
        const index2 = Number(index);
        let cls = this.#classes[index2];
        if(cls === undefined) {
            const protoClass = this.#program.classes[index2];
            if(protoClass === undefined) {
                throw new Error("Invalid class");
            }

            cls = this.#loadClass(protoClass);
            this.#classes[index2] = cls;
        }
        return cls;
    }

    getClassIndex(classInstance: VMClass): bigint {
        const index = this.#classes.indexOf(classInstance);
        if(index < 0) {
            throw new Error("Invalid class");
        }
        return BigInt(index);
    }

    #loadClass(cls: proto.Class): VMClass {
        const thisProgram = this;
        const baseClass = cls.baseClassId === undefined
            ? null
            : this.getClass(cls.baseClassId);

        let proto: any = null;
        

        return {
            get prototypeObject() {
                if(proto === null) {
                    proto = Object.create(baseClass == null ? null : baseClass.prototypeObject);
        
                    for(const impl of cls.implementations) {
                        let symbol: symbol;
                        switch(impl.declaringType.oneofKind) {
                            case "declaringClassId":
                            {
                                const cls = thisProgram.getClass(impl.declaringType.declaringClassId);
                                const slot = cls.slots[Number(impl.slotIndex)];
                                if(slot === undefined) {
                                    throw new Error("Invalid slot")
                                }

                                symbol = slot.symbol;
                                break;
                            }
            
                            case undefined: throw new Error("Invalid implementation declaring type");
                        }
            
                        const func = thisProgram.getFunction(impl.functionIndex);
                        proto[symbol] = func;
                    }
                }

                return proto;
            },

            baseClass,

            fields: cls.fields.map(field => ({
                fieldType: loadVMType(field.type!),
                symbol: Symbol(),
            })),

            slots: cls.slots.map(slot => ({
                parameterTypes: slot.parameterTypes.map(loadVMType),
                returnType: loadVMType(slot.returnType!),
                symbol: Symbol(),
            })),
        };
    }

}

function loadFunction(func: proto.Function, nativeFunctions: NativeFunctions): VMFunction {
    switch(func.func.oneofKind) {
        case "bytecode":
        {
            const bytecode = func.func.bytecode;

            return {
                native: false,
                parameterTypes: bytecode.parameterTypes.map(loadVMType),
                returnType: loadVMType(bytecode.returnType!),
                chunk: loadChunk(bytecode.body!),
            }
        }

        case "native":
        {
            const f = nativeFunctions[func.func.native];
            if(f === undefined) {
                throw new Error("Unknown native function: " + func.func.native);
            }

            return f;
        }

        case undefined: throw new Error("Invalid function type");
    }
}

