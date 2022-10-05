import { Chunk } from "@argon-lang/vm-format/lib/chunk.js";
import { Program } from "@argon-lang/vm-format/lib/program.js";
import { CallStackFrame, StackFrame } from "./stack.js";
import { Instruction } from "@argon-lang/vm-format/lib/instructions.js";
import { TaggedValue, VMType, TypeMapping, isSameType, getDefaultValue } from "@argon-lang/vm-format/lib/vmtype.js";
import { VMClass } from "@argon-lang/vm-format/lib/vmclass.js";
import { NativeTrampoline, VMFunction } from "@argon-lang/vm-format/lib/vmfunction.js";


type FrameExit = { type: "parent-frame"; frame: StackFrame; } | { type: "result"; value: unknown };

type TrampolinedFrameExit = FrameExit | { type: "delay"; resultType: VMType, trampoline: NativeTrampoline };


type TrampolineFrame = { native: false; frame: StackFrame } | { native: true; resultType: VMType, trampoline: NativeTrampoline };



export class VM {
    constructor(program: Program) {
        this.#program = program;
    }

    #program: Program;
    #callStack: CallStackFrame[] = [];

    async execute(chunk: Chunk): Promise<void> {
        const frame = StackFrame.create(chunk);
        return await this.#run(frame);
    }

    async #run(frame0: StackFrame): Promise<void> {
        let frame: StackFrame | null = frame0;
        while(frame != null) {
            const instruction = Instruction.read(frame);

            switch(instruction.op) {
                case "NOP": break;
                case "POP":
                    frame.pop();
                    break;

                case "RETURN":
                {
                    const result: TaggedValue = frame.pop();
                    const frameExit = await this.#returnValue(result);
                    switch(frameExit.type) {
                        case "parent-frame":
                            frame = frameExit.frame;
                            break;

                        case "result":
                            return;

                        default: unreachable(frameExit);
                    }
                    break;
                }

                case "CONSTANT":
                {
                    const c = frame.chunk.constants[Number(instruction.index)];
                    if(c === undefined) {
                        throw new Error("Invalid constant");
                    }

                    frame.push(c);
                    break;
                }

                case "CONSTANT_0_INT32":
                    frame.push({ type: VMType.Int32, value: 0 });
                    break;

                case "CONSTANT_1_INT32":
                    frame.push({ type: VMType.Int32, value: 1 });
                    break;

                case "CONSTANT_NULL":
                    frame.push({ type: VMType.ObjectReference, value: null });
                    break;
                    
                case "NEGATE":
                    unaryOpResOperand(frame, {
                        int8(a) { return -a; },
                        int16(a) { return -a; },
                        int32(a) { return -a; },
                        int64(a) { return -a; },
                        float32(a) { return -a; },
                        float64(a) { return -a; },
                    });
                    break;

                case "ADD":
                    binaryOpResOperand(frame, {
                        int8(a, b) { return a + b; },
                        int16(a, b) { return a + b; },
                        int32(a, b) { return a + b; },
                        int64(a, b) { return a + b; },
                        float32(a, b) { return a + b; },
                        float64(a, b) { return a + b; },
                    });
                    break;

                case "SUBTRACT":
                    binaryOpResOperand(frame, {
                        int8(a, b) { return a - b; },
                        int16(a, b) { return a - b; },
                        int32(a, b) { return a - b; },
                        int64(a, b) { return a - b; },
                        float32(a, b) { return a - b; },
                        float64(a, b) { return a - b; },
                    });
                    break;
    
                case "MULTIPLY":
                    binaryOpResOperand(frame, {
                        int8(a, b) { return Math.imul(a, b); },
                        int16(a, b) { return Math.imul(a, b); },
                        int32(a, b) { return Math.imul(a, b); },
                        int64(a, b) { return a * b; },
                        float32(a, b) { return a * b; },
                        float64(a, b) { return a * b; },
                    });
                    break;
                
                case "DIVIDE":
                    binaryOpResOperand(frame, {
                        int8(a, b) { return a / b; },
                        int16(a, b) { return a / b; },
                        int32(a, b) { return a / b; },
                        int64(a, b) { return a / b; },
                        float32(a, b) { return a / b; },
                        float64(a, b) { return a / b; },
                    });
                    break;

                
                case "DIVIDE_UN":
                    binaryOpResOperand(frame, {
                        int8(a, b) { return (a & 0xFF) / (b & 0xFF); },
                        int16(a, b) { return (a & 0xFFFF) / (b & 0xFFFF); },
                        int32(a, b) { return (a >>> 0) / (b >>> 0); },
                        int64(a, b) { return BigInt.asUintN(64, a) / BigInt.asUintN(64, b); },
                    });
                    break;

                case "EQZ":
                    frame.push({
                        type: VMType.Int32,
                        value: valueIsZero(frame.pop()) ? 1 : 0,
                    });
                    break;
                
                case "NEZ":
                    frame.push({
                        type: VMType.Int32,
                        value: valueIsZero(frame.pop()) ? 0 : 1,
                    });
                    break;
                    
                case "EQ":
                {
                    const b = frame.pop();
                    const a = frame.pop();
                    frame.push({
                        type: VMType.Int32,
                        value: valuesAreEqual(a, b) ? 1 : 0,
                    });
                    break;
                }
                 
                case "NE":
                {
                    const b = frame.pop();
                    const a = frame.pop();
                    frame.push({
                        type: VMType.Int32,
                        value: valuesAreEqual(a, b) ? 0 : 1,
                    });
                    break;
                }

                case "LT":
                    binaryOpResBool(frame, {
                        int8(a, b) { return a < b; },
                        int16(a, b) { return a < b; },
                        int32(a, b) { return a < b; },
                        int64(a, b) { return a < b; },
                        float32(a, b) { return a < b; },
                        float64(a, b) { return a < b; },
                    });
                    break;

                case "LT_UN":
                    binaryOpResBool(frame, {
                        int8(a, b) { return (a & 0xFF) < (b & 0xFF); },
                        int16(a, b) { return (a & 0xFFFF) < (b & 0xFFFF); },
                        int32(a, b) { return (a >>> 0) < (b >>> 0); },
                        int64(a, b) { return BigInt.asUintN(64, a) < BigInt.asUintN(64, b); },
                        float32(a, b) { return !(a >= b); },
                        float64(a, b) { return !(a >= b); },
                    });
                    break;

                case "LE":
                    binaryOpResBool(frame, {
                        int8(a, b) { return a <= b; },
                        int16(a, b) { return a <= b; },
                        int32(a, b) { return a <= b; },
                        int64(a, b) { return a <= b; },
                        float32(a, b) { return a <= b; },
                        float64(a, b) { return a <= b; },
                    });
                    break;

                case "LE_UN":
                    binaryOpResBool(frame, {
                        int8(a, b) { return (a & 0xFF) <= (b & 0xFF); },
                        int16(a, b) { return (a & 0xFFFF) <= (b & 0xFFFF); },
                        int32(a, b) { return (a >>> 0) <= (b >>> 0); },
                        int64(a, b) { return BigInt.asUintN(64, a) <= BigInt.asUintN(64, b); },
                        float32(a, b) { return !(a > b); },
                        float64(a, b) { return !(a > b); },
                    });
                    break;

                case "GT":
                    binaryOpResBool(frame, {
                        int8(a, b) { return a > b; },
                        int16(a, b) { return a > b; },
                        int32(a, b) { return a > b; },
                        int64(a, b) { return a > b; },
                        float32(a, b) { return a > b; },
                        float64(a, b) { return a > b; },
                    });
                    break;

                case "GT_UN":
                    binaryOpResBool(frame, {
                        int8(a, b) { return (a & 0xFF) > (b & 0xFF); },
                        int16(a, b) { return (a & 0xFFFF) > (b & 0xFFFF); },
                        int32(a, b) { return (a >>> 0) > (b >>> 0); },
                        int64(a, b) { return BigInt.asUintN(64, a) > BigInt.asUintN(64, b); },
                        float32(a, b) { return !(a <= b); },
                        float64(a, b) { return !(a <= b); },
                    });
                    break;

                case "GE":
                    binaryOpResBool(frame, {
                        int8(a, b) { return a >= b; },
                        int16(a, b) { return a >= b; },
                        int32(a, b) { return a >= b; },
                        int64(a, b) { return a >= b; },
                        float32(a, b) { return a >= b; },
                        float64(a, b) { return a >= b; },
                    });
                    break;

                case "GE_UN":
                    binaryOpResBool(frame, {
                        int8(a, b) { return (a & 0xFF) >= (b & 0xFF); },
                        int16(a, b) { return (a & 0xFFFF) >= (b & 0xFFFF); },
                        int32(a, b) { return (a >>> 0) >= (b >>> 0); },
                        int64(a, b) { return BigInt.asUintN(64, a) >= BigInt.asUintN(64, b); },
                        float32(a, b) { return !(a < b); },
                        float64(a, b) { return !(a < b); },
                    });
                    break;

                case "JMP":
                {
                    const target = frame.chunk.labelTargets[Number(instruction.label)];
                    if(target === undefined) {
                        throw new Error("Invalid label");
                    }
                    frame.ip = Number(target);
                    break;
                }

                case "JZ":
                {
                    const target = frame.chunk.labelTargets[Number(instruction.label)];
                    if(target === undefined) {
                        throw new Error("Invalid label");
                    }

                    const a = frame.pop();
                    if(valueIsZero(a)) {
                        frame.ip = Number(target);
                    }
                    break;
                }
                    
                case "JNZ":
                {
                    const target = frame.chunk.labelTargets[Number(instruction.label)];
                    if(target === undefined) {
                        throw new Error("Invalid label");
                    }

                    const a = frame.pop();
                    if(!valueIsZero(a)) {
                        frame.ip = Number(target);
                    }
                    break;
                }

                case "CALL":
                    frame = await this.#callImpl(frame, this.#program.getFunction(instruction.index), false);
                    break;

                case "RETURN_CALL":
                    frame = await this.#callImpl(frame, this.#program.getFunction(instruction.index), true);
                    break;

                case "LD_LOCAL":
                {
                    const value = frame.variables[Number(instruction.index)];
                    if(value === undefined) {
                        throw new Error("Invalid variable");
                    }

                    frame.push(value);
                    break;
                }

                case "ST_LOCAL":
                    frame.variables[Number(instruction.index)] = frame.pop();
                    break;

                case "NEW":
                {
                    const classObj = this.#program.getClass(instruction.class_index);
                    const obj: any = Object.create(classObj.prototypeObject);

                    for(let cls: VMClass | null = classObj; cls != null; cls = cls.baseClass) {
                        for(const field of cls.fields) {
                            obj[field.symbol] = getDefaultValue(field.fieldType);
                        }
                    }

                    frame.push({
                        type: VMType.ObjectReference,
                        value: obj,
                    });
                    break;
                }

                case "ST_FIELD":
                {
                    const cls = this.#program.getClass(instruction.class_index);
                    const field = cls.fields[Number(instruction.field_index)];
                    if(field === undefined) {
                        throw new Error("Unknown field");
                    }

                    const value = frame.pop();
                    const obj = frame.pop();

                    (obj.value as any)[field.symbol] = value.value;
                    break;
                }

                case "LD_FIELD":
                {
                    const cls = this.#program.getClass(instruction.class_index);
                    const field = cls.fields[Number(instruction.field_index)];
                    if(field === undefined) {
                        throw new Error("Unknown field");
                    }

                    const obj = frame.pop();

                    frame.push({
                        type: field.fieldType,
                        value: (obj.value as any)[field.symbol]
                    });
                    break;
                }

                case "CALL_CLASS":
                {
                    const cls = this.#program.getClass(instruction.class_index);
                    const slot = cls.slots[Number(instruction.slot_index)];
                    if(slot === undefined) {
                        throw new Error("Unknown slot");
                    }
                    const thisValue: TaggedValue | undefined = frame.stack[frame.stack.length - slot.parameterTypes.length];
                    if(thisValue === undefined) {
                        throw new Error("Stack underflow");
                    }
                    
                    
                    const func: VMFunction = (thisValue.value as any)[slot.symbol];
                    frame = await this.#callImpl(frame, func, false);
                    break;
                }

                case "RETURN_CALL_CLASS":
                {
                    const cls = this.#program.getClass(instruction.class_index);
                    const slot = cls.slots[Number(instruction.slot_index)];
                    if(slot === undefined) {
                        throw new Error("Unknown slot");
                    }
                    const thisValue: TaggedValue | undefined = frame.stack[frame.stack.length - slot.parameterTypes.length];
                    if(thisValue === undefined) {
                        throw new Error("Stack underflow");
                    }
                    
                    
                    const func: VMFunction = (thisValue.value as any)[slot.symbol];
                    frame = await this.#callImpl(frame, func, true);
                    break;
                }

                default: unreachable(instruction);
            }
        }
    }

    async #returnValue(value: TaggedValue): Promise<FrameExit> {
        let result = await this.#returnValueTramp(value);
        while(true) {
            switch(result.type) {
                case "delay":
                    result = await this.#runTrampolineStep({ native: true, resultType: result.resultType, trampoline: result.trampoline });
                    break;

                default: return result;
            }
        }
    }

    async #returnValueTramp(value: TaggedValue): Promise<TrampolinedFrameExit> {
        const frame = this.#callStack.pop();
        if(frame === undefined) {
            return { type: "result", value: value.value };
        }

        if(frame.native) {
            return { type: "delay", resultType: frame.resultType, trampoline: await frame.continuation(value.value) }
        }
        else {
            frame.push(value);
            return { type: "parent-frame", frame };
        }
    }

    async #runTrampolineStep(frame: TrampolineFrame): Promise<TrampolinedFrameExit> {
        while(true) {
            if(frame.native) {
                const trampoline = frame.trampoline;
                switch(trampoline.type) {
                    case "result":
                        return await this.#returnValueTramp({
                            type: frame.resultType,
                            value: trampoline.value,
                        });

                    case "delay-function":
                        this.#callStack.push({
                            native: true,
                            resultType: frame.resultType,
                            continuation(arg) {
                                return trampoline.continuation(arg)
                            },
                        });
                        frame = await this.#callFunction(trampoline.next, trampoline.args);
                        break;

                    case "delay":
                        frame = { native: true, resultType: frame.resultType, trampoline: await trampoline.invoke() };
                        break;

                        
                    default: unreachable(trampoline);

                }
            }
            else {
                return { type: "parent-frame", frame: frame.frame };
            }
        }
    }

    async #callImpl(frame: StackFrame, func: VMFunction, dropCurrentFrame: boolean): Promise<StackFrame | null> {
        if(func.parameterTypes.length > frame.stack.length) {
            throw new Error("Stack underflow");
        }

        const args = frame.stack.splice(frame.stack.length - func.parameterTypes.length).map(x => x.value);

        if(!dropCurrentFrame) {
            this.#callStack.push(frame);
        }

        const trampoline = await this.#callFunction(func, args);
        const frameExit = await this.#runTrampoline(trampoline);
        switch(frameExit.type) {
            case "result":
                return null;

            case "parent-frame":
                return frameExit.frame;
        }
    }

    async #callFunction(func: VMFunction, args: readonly unknown[]): Promise<TrampolineFrame> {
        if(func.native) {
            const trampoline = await func.invoke(...args);
            return { native: true, resultType: func.returnType, trampoline: trampoline };
        }
        else {
            if(args.length != func.parameterTypes.length) {
                throw new Error("Argument length mismatch");
            }

            const args2: TaggedValue[] = [];
            for(let i = 0; i < args.length; ++i) {
                args2.push({
                    type: func.parameterTypes[i]!,
                    value: args[i]!,
                });
            }

            return { native: false, frame: StackFrame.create(func.chunk, args2) };
        }
    }

    async #runTrampoline(frame: TrampolineFrame): Promise<FrameExit> {
        let step = await this.#runTrampolineStep(frame);
        while(true) {
            switch(step.type) {
                case "delay":
                    step = await this.#runTrampolineStep({ native: true, resultType: step.resultType, trampoline: step.trampoline });
                    break;

                default:
                    return step;
            }
        }
    }
}

function unreachable(dummy: never): never {
    return dummy;
}


type UnaryOperationResOperand = {
    [K in VMType["type"]]?: (a: TypeMapping[K]) => TypeMapping[K];
};

type UnaryOperationResBool = {
    [K in VMType["type"]]?: (a: TypeMapping[K]) => boolean;
};


type BinaryOperationResOperand = {
    [K in VMType["type"]]?: (a: TypeMapping[K], b: TypeMapping[K]) => TypeMapping[K];
};

type BinaryOperationResBool = {
    [K in VMType["type"]]?: (a: TypeMapping[K], b: TypeMapping[K]) => boolean;
};


const normalizeValue: Required<UnaryOperationResOperand> = {
    int8(a) { return a << 24 >> 24; },
    int16(a) { return a << 16 >> 16 },
    int32(a) { return a | 0 },
    int64(a) { return BigInt.asIntN(64, a); },
    float32(a) { return Math.fround(a); },
    float64(a) { return a; },
    tuple(a) { return a; },
    object_reference(a) { return a; },
}

function unaryOpResOperand(frame: StackFrame, ops: UnaryOperationResOperand): void {
    const a = frame.pop();
    const op = ops[a.type.type] as (a: any) => any;
    if(op === undefined) {
        throw new Error("Unsupported operation");
    }

    const normalize = normalizeValue[a.type.type] as (a: any) => any;

    const result = normalize(op(a.value));

    frame.push({
        type: a.type,
        value: result,
    });
}

function binaryOpResOperand(frame: StackFrame, ops: BinaryOperationResOperand): void {
    const b = frame.pop();
    const a = frame.pop();
    const op = ops[a.type.type] as (a: any, b: any) => any;
    if(op === undefined || !isSameType(a.type, b.type)) {
        throw new Error("Unsupported operation");
    }

    const normalize = normalizeValue[a.type.type] as (a: any) => any;

    const result = normalize(op(a.value, b.value));

    frame.push({
        type: a.type,
        value: result,
    });
}

function binaryOpResBool(frame: StackFrame, ops: BinaryOperationResBool): void {
    const b = frame.pop();
    const a = frame.pop();
    const op = ops[a.type.type] as (a: any, b: any) => boolean;
    if(op === undefined || !isSameType(a.type, b.type)) {
        throw new Error("Unsupported operation");
    }

    const result = op(a.value, b.value);

    frame.push({
        type: VMType.Int32,
        value: result ? 1 : 0,
    });
}

function valueIsZero(value: TaggedValue): boolean {
    switch(value.type.type) {
        case "int8":
        case "int16":
        case "int32":
        case "float32":
        case "float64":
            return value.value === 0;

        case "int64":
            return value.value === 0n;

        case "tuple":
            return false;

        case "object_reference":
            return value.value === null;
    }
}

function valuesAreEqual(a: TaggedValue, b: TaggedValue): boolean {
    if(!isSameType(a.type, b.type)) {
        return false;
    }

    if(a.type.type === "tuple" && b.type.type === "tuple") {
        const aElems = a.value as readonly unknown[];
        const bElems = b.value as readonly unknown[];
        for(let i = 0; i < aElems.length; ++i) {
            const t = a.type.elements[i]!;
            if(!valuesAreEqual({ type: t, value: aElems[i]! }, { type: t, value: bElems[i]! })) {
                return false;
            }
        }
        return true;
    }
    else {
        return a.value === b.value;
    }
}

