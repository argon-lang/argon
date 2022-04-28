import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ExpressionWithVariables from "./ExpressionWithVariables.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export type V1 = { readonly tag: "expressionBody", readonly expressionBody: sym_argon_tube_ExpressionWithVariables.V1, }
	| { readonly tag: "externalImplementation", readonly externalImplementation: sym_unit.Unit, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "expressionBody", expressionBody: await sym_argon_tube_ExpressionWithVariables.V1.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "externalImplementation", externalImplementation: await sym_unit.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "expressionBody":
				{
					const expressionBody = value.expressionBody;
					await natCodec.write(writer, 0n);
					await sym_argon_tube_ExpressionWithVariables.V1.codec.write(writer, expressionBody);
					break;
				}
				case "externalImplementation":
				{
					const externalImplementation = value.externalImplementation;
					await natCodec.write(writer, 1n);
					await sym_unit.codec.write(writer, externalImplementation);
					break;
				}
			}
		},
	};
}
