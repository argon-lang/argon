import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_int from "@verilization/runtime/int.js";
export type V1 = { readonly tag: "byClass", readonly byClass: sym_int.Int, }
	| { readonly tag: "byClassStatic", readonly byClassStatic: sym_int.Int, }
	| { readonly tag: "byTrait", readonly byTrait: sym_int.Int, }
	| { readonly tag: "byTraitStatic", readonly byTraitStatic: sym_int.Int, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "byClass", byClass: await sym_int.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "byClassStatic", byClassStatic: await sym_int.codec.read(reader)};
				}
				case 2n:
				{
					return { tag: "byTrait", byTrait: await sym_int.codec.read(reader)};
				}
				case 3n:
				{
					return { tag: "byTraitStatic", byTraitStatic: await sym_int.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "byClass":
				{
					const byClass = value.byClass;
					await natCodec.write(writer, 0n);
					await sym_int.codec.write(writer, byClass);
					break;
				}
				case "byClassStatic":
				{
					const byClassStatic = value.byClassStatic;
					await natCodec.write(writer, 1n);
					await sym_int.codec.write(writer, byClassStatic);
					break;
				}
				case "byTrait":
				{
					const byTrait = value.byTrait;
					await natCodec.write(writer, 2n);
					await sym_int.codec.write(writer, byTrait);
					break;
				}
				case "byTraitStatic":
				{
					const byTraitStatic = value.byTraitStatic;
					await natCodec.write(writer, 3n);
					await sym_int.codec.write(writer, byTraitStatic);
					break;
				}
			}
		},
	};
}
