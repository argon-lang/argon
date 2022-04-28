import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_unit from "@verilization/runtime/unit.js";
export type V1 = { readonly tag: "mutable", readonly mutable: sym_unit.Unit, }
	| { readonly tag: "nonMutable", readonly nonMutable: sym_unit.Unit, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "mutable", mutable: await sym_unit.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "nonMutable", nonMutable: await sym_unit.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "mutable":
				{
					const mutable = value.mutable;
					await natCodec.write(writer, 0n);
					await sym_unit.codec.write(writer, mutable);
					break;
				}
				case "nonMutable":
				{
					const nonMutable = value.nonMutable;
					await natCodec.write(writer, 1n);
					await sym_unit.codec.write(writer, nonMutable);
					break;
				}
			}
		},
	};
}
