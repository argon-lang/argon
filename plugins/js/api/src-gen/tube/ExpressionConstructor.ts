import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_PatternExpr from "./PatternExpr.js";
import * as sym_argon_tube_Variable from "./Variable.js";
import * as sym_bool from "@verilization/runtime/bool.js";
import * as sym_int from "@verilization/runtime/int.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_string from "@verilization/runtime/string.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export type V1 = { readonly tag: "bindVariable", readonly bindVariable: sym_nat.Nat, }
	| { readonly tag: "classConstructorCall", readonly classConstructorCall: sym_int.Int, }
	| { readonly tag: "ensureExecuted", readonly ensureExecuted: sym_unit.Unit, }
	| { readonly tag: "functionCall", readonly functionCall: sym_int.Int, }
	| { readonly tag: "functionObjectCall", readonly functionObjectCall: sym_unit.Unit, }
	| { readonly tag: "ifElse", readonly ifElse: sym_unit.Unit, }
	| { readonly tag: "loadConstantBool", readonly loadConstantBool: sym_bool.Bool, }
	| { readonly tag: "loadConstantInt", readonly loadConstantInt: sym_int.Int, }
	| { readonly tag: "loadConstantString", readonly loadConstantString: sym_string.String, }
	| { readonly tag: "loadLambda", readonly loadLambda: sym_nat.Nat, }
	| { readonly tag: "loadTuple", readonly loadTuple: sym_unit.Unit, }
	| { readonly tag: "loadTupleElement", readonly loadTupleElement: sym_nat.Nat, }
	| { readonly tag: "loadVariable", readonly loadVariable: sym_argon_tube_Variable.V1, }
	| { readonly tag: "methodCall", readonly methodCall: sym_int.Int, }
	| { readonly tag: "patternMatch", readonly patternMatch: sym_list.List<sym_argon_tube_PatternExpr.V1>, }
	| { readonly tag: "raiseException", readonly raiseException: sym_unit.Unit, }
	| { readonly tag: "sequence", readonly sequence: sym_unit.Unit, }
	| { readonly tag: "storeVariable", readonly storeVariable: sym_argon_tube_Variable.V1, }
	| { readonly tag: "typeN", readonly typeN: sym_unit.Unit, }
	| { readonly tag: "omegaTypeN", readonly omegaTypeN: sym_nat.Nat, }
	| { readonly tag: "anyType", readonly anyType: sym_unit.Unit, }
	| { readonly tag: "traitType", readonly traitType: sym_int.Int, }
	| { readonly tag: "classType", readonly classType: sym_int.Int, }
	| { readonly tag: "functionType", readonly functionType: sym_unit.Unit, }
	| { readonly tag: "unionType", readonly unionType: sym_unit.Unit, }
	| { readonly tag: "intersectionType", readonly intersectionType: sym_unit.Unit, }
	| { readonly tag: "existentialType", readonly existentialType: sym_nat.Nat, }
	| { readonly tag: "conjunctionType", readonly conjunctionType: sym_unit.Unit, }
	| { readonly tag: "disjunctionType", readonly disjunctionType: sym_unit.Unit, }
	| { readonly tag: "neverType", readonly neverType: sym_unit.Unit, }
	| { readonly tag: "subtypeWitnessType", readonly subtypeWitnessType: sym_unit.Unit, }
	| { readonly tag: "equalTo", readonly equalTo: sym_unit.Unit, }
	| { readonly tag: "assumeErasedValue", readonly assumeErasedValue: sym_unit.Unit, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "bindVariable", bindVariable: await sym_nat.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "classConstructorCall", classConstructorCall: await sym_int.codec.read(reader)};
				}
				case 2n:
				{
					return { tag: "ensureExecuted", ensureExecuted: await sym_unit.codec.read(reader)};
				}
				case 3n:
				{
					return { tag: "functionCall", functionCall: await sym_int.codec.read(reader)};
				}
				case 4n:
				{
					return { tag: "functionObjectCall", functionObjectCall: await sym_unit.codec.read(reader)};
				}
				case 5n:
				{
					return { tag: "ifElse", ifElse: await sym_unit.codec.read(reader)};
				}
				case 6n:
				{
					return { tag: "loadConstantBool", loadConstantBool: await sym_bool.codec.read(reader)};
				}
				case 7n:
				{
					return { tag: "loadConstantInt", loadConstantInt: await sym_int.codec.read(reader)};
				}
				case 8n:
				{
					return { tag: "loadConstantString", loadConstantString: await sym_string.codec.read(reader)};
				}
				case 9n:
				{
					return { tag: "loadLambda", loadLambda: await sym_nat.codec.read(reader)};
				}
				case 10n:
				{
					return { tag: "loadTuple", loadTuple: await sym_unit.codec.read(reader)};
				}
				case 11n:
				{
					return { tag: "loadTupleElement", loadTupleElement: await sym_nat.codec.read(reader)};
				}
				case 12n:
				{
					return { tag: "loadVariable", loadVariable: await sym_argon_tube_Variable.V1.codec.read(reader)};
				}
				case 13n:
				{
					return { tag: "methodCall", methodCall: await sym_int.codec.read(reader)};
				}
				case 14n:
				{
					return { tag: "patternMatch", patternMatch: await sym_list.codec<sym_argon_tube_PatternExpr.V1>(sym_argon_tube_PatternExpr.V1.codec).read(reader)};
				}
				case 15n:
				{
					return { tag: "raiseException", raiseException: await sym_unit.codec.read(reader)};
				}
				case 16n:
				{
					return { tag: "sequence", sequence: await sym_unit.codec.read(reader)};
				}
				case 17n:
				{
					return { tag: "storeVariable", storeVariable: await sym_argon_tube_Variable.V1.codec.read(reader)};
				}
				case 18n:
				{
					return { tag: "typeN", typeN: await sym_unit.codec.read(reader)};
				}
				case 19n:
				{
					return { tag: "omegaTypeN", omegaTypeN: await sym_nat.codec.read(reader)};
				}
				case 20n:
				{
					return { tag: "anyType", anyType: await sym_unit.codec.read(reader)};
				}
				case 21n:
				{
					return { tag: "traitType", traitType: await sym_int.codec.read(reader)};
				}
				case 22n:
				{
					return { tag: "classType", classType: await sym_int.codec.read(reader)};
				}
				case 23n:
				{
					return { tag: "functionType", functionType: await sym_unit.codec.read(reader)};
				}
				case 24n:
				{
					return { tag: "unionType", unionType: await sym_unit.codec.read(reader)};
				}
				case 25n:
				{
					return { tag: "intersectionType", intersectionType: await sym_unit.codec.read(reader)};
				}
				case 26n:
				{
					return { tag: "existentialType", existentialType: await sym_nat.codec.read(reader)};
				}
				case 27n:
				{
					return { tag: "conjunctionType", conjunctionType: await sym_unit.codec.read(reader)};
				}
				case 28n:
				{
					return { tag: "disjunctionType", disjunctionType: await sym_unit.codec.read(reader)};
				}
				case 29n:
				{
					return { tag: "neverType", neverType: await sym_unit.codec.read(reader)};
				}
				case 30n:
				{
					return { tag: "subtypeWitnessType", subtypeWitnessType: await sym_unit.codec.read(reader)};
				}
				case 31n:
				{
					return { tag: "equalTo", equalTo: await sym_unit.codec.read(reader)};
				}
				case 32n:
				{
					return { tag: "assumeErasedValue", assumeErasedValue: await sym_unit.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "bindVariable":
				{
					const bindVariable = value.bindVariable;
					await natCodec.write(writer, 0n);
					await sym_nat.codec.write(writer, bindVariable);
					break;
				}
				case "classConstructorCall":
				{
					const classConstructorCall = value.classConstructorCall;
					await natCodec.write(writer, 1n);
					await sym_int.codec.write(writer, classConstructorCall);
					break;
				}
				case "ensureExecuted":
				{
					const ensureExecuted = value.ensureExecuted;
					await natCodec.write(writer, 2n);
					await sym_unit.codec.write(writer, ensureExecuted);
					break;
				}
				case "functionCall":
				{
					const functionCall = value.functionCall;
					await natCodec.write(writer, 3n);
					await sym_int.codec.write(writer, functionCall);
					break;
				}
				case "functionObjectCall":
				{
					const functionObjectCall = value.functionObjectCall;
					await natCodec.write(writer, 4n);
					await sym_unit.codec.write(writer, functionObjectCall);
					break;
				}
				case "ifElse":
				{
					const ifElse = value.ifElse;
					await natCodec.write(writer, 5n);
					await sym_unit.codec.write(writer, ifElse);
					break;
				}
				case "loadConstantBool":
				{
					const loadConstantBool = value.loadConstantBool;
					await natCodec.write(writer, 6n);
					await sym_bool.codec.write(writer, loadConstantBool);
					break;
				}
				case "loadConstantInt":
				{
					const loadConstantInt = value.loadConstantInt;
					await natCodec.write(writer, 7n);
					await sym_int.codec.write(writer, loadConstantInt);
					break;
				}
				case "loadConstantString":
				{
					const loadConstantString = value.loadConstantString;
					await natCodec.write(writer, 8n);
					await sym_string.codec.write(writer, loadConstantString);
					break;
				}
				case "loadLambda":
				{
					const loadLambda = value.loadLambda;
					await natCodec.write(writer, 9n);
					await sym_nat.codec.write(writer, loadLambda);
					break;
				}
				case "loadTuple":
				{
					const loadTuple = value.loadTuple;
					await natCodec.write(writer, 10n);
					await sym_unit.codec.write(writer, loadTuple);
					break;
				}
				case "loadTupleElement":
				{
					const loadTupleElement = value.loadTupleElement;
					await natCodec.write(writer, 11n);
					await sym_nat.codec.write(writer, loadTupleElement);
					break;
				}
				case "loadVariable":
				{
					const loadVariable = value.loadVariable;
					await natCodec.write(writer, 12n);
					await sym_argon_tube_Variable.V1.codec.write(writer, loadVariable);
					break;
				}
				case "methodCall":
				{
					const methodCall = value.methodCall;
					await natCodec.write(writer, 13n);
					await sym_int.codec.write(writer, methodCall);
					break;
				}
				case "patternMatch":
				{
					const patternMatch = value.patternMatch;
					await natCodec.write(writer, 14n);
					await sym_list.codec<sym_argon_tube_PatternExpr.V1>(sym_argon_tube_PatternExpr.V1.codec).write(writer, patternMatch);
					break;
				}
				case "raiseException":
				{
					const raiseException = value.raiseException;
					await natCodec.write(writer, 15n);
					await sym_unit.codec.write(writer, raiseException);
					break;
				}
				case "sequence":
				{
					const sequence = value.sequence;
					await natCodec.write(writer, 16n);
					await sym_unit.codec.write(writer, sequence);
					break;
				}
				case "storeVariable":
				{
					const storeVariable = value.storeVariable;
					await natCodec.write(writer, 17n);
					await sym_argon_tube_Variable.V1.codec.write(writer, storeVariable);
					break;
				}
				case "typeN":
				{
					const typeN = value.typeN;
					await natCodec.write(writer, 18n);
					await sym_unit.codec.write(writer, typeN);
					break;
				}
				case "omegaTypeN":
				{
					const omegaTypeN = value.omegaTypeN;
					await natCodec.write(writer, 19n);
					await sym_nat.codec.write(writer, omegaTypeN);
					break;
				}
				case "anyType":
				{
					const anyType = value.anyType;
					await natCodec.write(writer, 20n);
					await sym_unit.codec.write(writer, anyType);
					break;
				}
				case "traitType":
				{
					const traitType = value.traitType;
					await natCodec.write(writer, 21n);
					await sym_int.codec.write(writer, traitType);
					break;
				}
				case "classType":
				{
					const classType = value.classType;
					await natCodec.write(writer, 22n);
					await sym_int.codec.write(writer, classType);
					break;
				}
				case "functionType":
				{
					const functionType = value.functionType;
					await natCodec.write(writer, 23n);
					await sym_unit.codec.write(writer, functionType);
					break;
				}
				case "unionType":
				{
					const unionType = value.unionType;
					await natCodec.write(writer, 24n);
					await sym_unit.codec.write(writer, unionType);
					break;
				}
				case "intersectionType":
				{
					const intersectionType = value.intersectionType;
					await natCodec.write(writer, 25n);
					await sym_unit.codec.write(writer, intersectionType);
					break;
				}
				case "existentialType":
				{
					const existentialType = value.existentialType;
					await natCodec.write(writer, 26n);
					await sym_nat.codec.write(writer, existentialType);
					break;
				}
				case "conjunctionType":
				{
					const conjunctionType = value.conjunctionType;
					await natCodec.write(writer, 27n);
					await sym_unit.codec.write(writer, conjunctionType);
					break;
				}
				case "disjunctionType":
				{
					const disjunctionType = value.disjunctionType;
					await natCodec.write(writer, 28n);
					await sym_unit.codec.write(writer, disjunctionType);
					break;
				}
				case "neverType":
				{
					const neverType = value.neverType;
					await natCodec.write(writer, 29n);
					await sym_unit.codec.write(writer, neverType);
					break;
				}
				case "subtypeWitnessType":
				{
					const subtypeWitnessType = value.subtypeWitnessType;
					await natCodec.write(writer, 30n);
					await sym_unit.codec.write(writer, subtypeWitnessType);
					break;
				}
				case "equalTo":
				{
					const equalTo = value.equalTo;
					await natCodec.write(writer, 31n);
					await sym_unit.codec.write(writer, equalTo);
					break;
				}
				case "assumeErasedValue":
				{
					const assumeErasedValue = value.assumeErasedValue;
					await natCodec.write(writer, 32n);
					await sym_unit.codec.write(writer, assumeErasedValue);
					break;
				}
			}
		},
	};
}
