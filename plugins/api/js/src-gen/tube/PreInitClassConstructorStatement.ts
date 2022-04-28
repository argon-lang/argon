import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_FieldInitializer from "./FieldInitializer.js";
export type V1 = { readonly tag: "expression", readonly expression: sym_argon_tube_Expression.V1, }
	| { readonly tag: "fieldInitializer", readonly fieldInitializer: sym_argon_tube_FieldInitializer.V1, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "expression", expression: await sym_argon_tube_Expression.V1.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "fieldInitializer", fieldInitializer: await sym_argon_tube_FieldInitializer.V1.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "expression":
				{
					const expression = value.expression;
					await natCodec.write(writer, 0n);
					await sym_argon_tube_Expression.V1.codec.write(writer, expression);
					break;
				}
				case "fieldInitializer":
				{
					const fieldInitializer = value.fieldInitializer;
					await natCodec.write(writer, 1n);
					await sym_argon_tube_FieldInitializer.V1.codec.write(writer, fieldInitializer);
					break;
				}
			}
		},
	};
}
