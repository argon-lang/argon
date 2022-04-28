import { Codec } from "@verilization/runtime";
import * as sym_argon_tube_PatternExpr from "./PatternExpr.js";
import * as sym_argon_tube_Variable from "./Variable.js";
import * as sym_bool from "@verilization/runtime/bool.js";
import * as sym_int from "@verilization/runtime/int.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_string from "@verilization/runtime/string.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export declare type V1 = {
    readonly tag: "bindVariable";
    readonly bindVariable: sym_nat.Nat;
} | {
    readonly tag: "classConstructorCall";
    readonly classConstructorCall: sym_int.Int;
} | {
    readonly tag: "ensureExecuted";
    readonly ensureExecuted: sym_unit.Unit;
} | {
    readonly tag: "functionCall";
    readonly functionCall: sym_int.Int;
} | {
    readonly tag: "functionObjectCall";
    readonly functionObjectCall: sym_unit.Unit;
} | {
    readonly tag: "ifElse";
    readonly ifElse: sym_unit.Unit;
} | {
    readonly tag: "loadConstantBool";
    readonly loadConstantBool: sym_bool.Bool;
} | {
    readonly tag: "loadConstantInt";
    readonly loadConstantInt: sym_int.Int;
} | {
    readonly tag: "loadConstantString";
    readonly loadConstantString: sym_string.String;
} | {
    readonly tag: "loadLambda";
    readonly loadLambda: sym_nat.Nat;
} | {
    readonly tag: "loadTuple";
    readonly loadTuple: sym_unit.Unit;
} | {
    readonly tag: "loadTupleElement";
    readonly loadTupleElement: sym_nat.Nat;
} | {
    readonly tag: "loadVariable";
    readonly loadVariable: sym_argon_tube_Variable.V1;
} | {
    readonly tag: "methodCall";
    readonly methodCall: sym_int.Int;
} | {
    readonly tag: "patternMatch";
    readonly patternMatch: sym_list.List<sym_argon_tube_PatternExpr.V1>;
} | {
    readonly tag: "raiseException";
    readonly raiseException: sym_unit.Unit;
} | {
    readonly tag: "sequence";
    readonly sequence: sym_unit.Unit;
} | {
    readonly tag: "storeVariable";
    readonly storeVariable: sym_argon_tube_Variable.V1;
} | {
    readonly tag: "typeN";
    readonly typeN: sym_unit.Unit;
} | {
    readonly tag: "omegaTypeN";
    readonly omegaTypeN: sym_nat.Nat;
} | {
    readonly tag: "anyType";
    readonly anyType: sym_unit.Unit;
} | {
    readonly tag: "traitType";
    readonly traitType: sym_int.Int;
} | {
    readonly tag: "classType";
    readonly classType: sym_int.Int;
} | {
    readonly tag: "functionType";
    readonly functionType: sym_unit.Unit;
} | {
    readonly tag: "unionType";
    readonly unionType: sym_unit.Unit;
} | {
    readonly tag: "intersectionType";
    readonly intersectionType: sym_unit.Unit;
} | {
    readonly tag: "existentialType";
    readonly existentialType: sym_nat.Nat;
} | {
    readonly tag: "conjunctionType";
    readonly conjunctionType: sym_unit.Unit;
} | {
    readonly tag: "disjunctionType";
    readonly disjunctionType: sym_unit.Unit;
} | {
    readonly tag: "neverType";
    readonly neverType: sym_unit.Unit;
} | {
    readonly tag: "subtypeWitnessType";
    readonly subtypeWitnessType: sym_unit.Unit;
} | {
    readonly tag: "equalTo";
    readonly equalTo: sym_unit.Unit;
} | {
    readonly tag: "assumeErasedValue";
    readonly assumeErasedValue: sym_unit.Unit;
};
export declare namespace V1 {
    const codec: Codec<V1>;
}
