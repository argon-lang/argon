import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_unit from "@verilization/runtime/unit.js";
export type V1 = { readonly tag: "normalList", readonly normalList: sym_unit.Unit, }
	| { readonly tag: "inferrableList", readonly inferrableList: sym_unit.Unit, }
	| { readonly tag: "inferrableList2", readonly inferrableList2: sym_unit.Unit, }
	| { readonly tag: "requiresList", readonly requiresList: sym_unit.Unit, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "normalList", normalList: await sym_unit.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "inferrableList", inferrableList: await sym_unit.codec.read(reader)};
				}
				case 2n:
				{
					return { tag: "inferrableList2", inferrableList2: await sym_unit.codec.read(reader)};
				}
				case 3n:
				{
					return { tag: "requiresList", requiresList: await sym_unit.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "normalList":
				{
					const normalList = value.normalList;
					await natCodec.write(writer, 0n);
					await sym_unit.codec.write(writer, normalList);
					break;
				}
				case "inferrableList":
				{
					const inferrableList = value.inferrableList;
					await natCodec.write(writer, 1n);
					await sym_unit.codec.write(writer, inferrableList);
					break;
				}
				case "inferrableList2":
				{
					const inferrableList2 = value.inferrableList2;
					await natCodec.write(writer, 2n);
					await sym_unit.codec.write(writer, inferrableList2);
					break;
				}
				case "requiresList":
				{
					const requiresList = value.requiresList;
					await natCodec.write(writer, 3n);
					await sym_unit.codec.write(writer, requiresList);
					break;
				}
			}
		},
	};
}
