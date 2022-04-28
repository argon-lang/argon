import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_bool from "@verilization/runtime/bool.js";
export type V1 = { readonly tag: "isPure", readonly isPure: sym_bool.Bool, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "isPure", isPure: await sym_bool.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "isPure":
				{
					const isPure = value.isPure;
					await natCodec.write(writer, 0n);
					await sym_bool.codec.write(writer, isPure);
					break;
				}
			}
		},
	};
}
