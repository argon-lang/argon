import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_unit from "@verilization/runtime/unit.js";
export type V1 = { readonly tag: "interface", readonly interface: sym_unit.Unit, }
	| { readonly tag: "definition", readonly definition: sym_unit.Unit, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "interface", interface: await sym_unit.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "definition", definition: await sym_unit.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "interface":
				{
					const _interface = value.interface;
					await natCodec.write(writer, 0n);
					await sym_unit.codec.write(writer, _interface);
					break;
				}
				case "definition":
				{
					const definition = value.definition;
					await natCodec.write(writer, 1n);
					await sym_unit.codec.write(writer, definition);
					break;
				}
			}
		},
	};
}
