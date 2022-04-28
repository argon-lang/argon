import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_nat from "@verilization/runtime/nat.js";
export type V1 = { readonly tag: "binding", readonly binding: sym_nat.Nat, }
	| { readonly tag: "castBinding", readonly castBinding: sym_nat.Nat, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "binding", binding: await sym_nat.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "castBinding", castBinding: await sym_nat.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "binding":
				{
					const binding = value.binding;
					await natCodec.write(writer, 0n);
					await sym_nat.codec.write(writer, binding);
					break;
				}
				case "castBinding":
				{
					const castBinding = value.castBinding;
					await natCodec.write(writer, 1n);
					await sym_nat.codec.write(writer, castBinding);
					break;
				}
			}
		},
	};
}
