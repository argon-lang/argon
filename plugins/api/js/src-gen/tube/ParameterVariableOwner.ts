import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_nat from "@verilization/runtime/nat.js";
export type V1 = { readonly tag: "byMethod", readonly byMethod: sym_nat.Nat, }
	| { readonly tag: "byFunction", readonly byFunction: sym_nat.Nat, }
	| { readonly tag: "byClass", readonly byClass: sym_nat.Nat, }
	| { readonly tag: "byTrait", readonly byTrait: sym_nat.Nat, }
	| { readonly tag: "byClassConstructor", readonly byClassConstructor: sym_nat.Nat, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "byMethod", byMethod: await sym_nat.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "byFunction", byFunction: await sym_nat.codec.read(reader)};
				}
				case 2n:
				{
					return { tag: "byClass", byClass: await sym_nat.codec.read(reader)};
				}
				case 3n:
				{
					return { tag: "byTrait", byTrait: await sym_nat.codec.read(reader)};
				}
				case 4n:
				{
					return { tag: "byClassConstructor", byClassConstructor: await sym_nat.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "byMethod":
				{
					const byMethod = value.byMethod;
					await natCodec.write(writer, 0n);
					await sym_nat.codec.write(writer, byMethod);
					break;
				}
				case "byFunction":
				{
					const byFunction = value.byFunction;
					await natCodec.write(writer, 1n);
					await sym_nat.codec.write(writer, byFunction);
					break;
				}
				case "byClass":
				{
					const byClass = value.byClass;
					await natCodec.write(writer, 2n);
					await sym_nat.codec.write(writer, byClass);
					break;
				}
				case "byTrait":
				{
					const byTrait = value.byTrait;
					await natCodec.write(writer, 3n);
					await sym_nat.codec.write(writer, byTrait);
					break;
				}
				case "byClassConstructor":
				{
					const byClassConstructor = value.byClassConstructor;
					await natCodec.write(writer, 4n);
					await sym_nat.codec.write(writer, byClassConstructor);
					break;
				}
			}
		},
	};
}
