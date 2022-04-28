import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ByModuleOwner from "./ByModuleOwner.js";
export type V1<Tube> = { readonly tag: "byModule", readonly byModule: sym_argon_tube_ByModuleOwner.V1<Tube>, };
export namespace V1 {
	export function codec<Tube>(Tube_codec: Codec<Tube>): Codec<V1<Tube>> {
		return {
			async read(reader: FormatReader): Promise<V1<Tube>> {
				switch(await natCodec.read(reader)) {
					case 0n:
					{
						return { tag: "byModule", byModule: await sym_argon_tube_ByModuleOwner.V1.codec<Tube>(Tube_codec).read(reader)};
					}
					default: throw new Error("Unknown tag");				}
			},
			async write(writer: FormatWriter, value: V1<Tube>): Promise<void> {
				switch(value.tag) {
					case "byModule":
					{
						const byModule = value.byModule;
						await natCodec.write(writer, 0n);
						await sym_argon_tube_ByModuleOwner.V1.codec<Tube>(Tube_codec).write(writer, byModule);
						break;
					}
				}
			},
		};
	}
}
