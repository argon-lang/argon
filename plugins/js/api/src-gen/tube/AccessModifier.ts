import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_unit from "@verilization/runtime/unit.js";
export type V1 = { readonly tag: "public", readonly public: sym_unit.Unit, }
	| { readonly tag: "tubePrivate", readonly tubePrivate: sym_unit.Unit, }
	| { readonly tag: "filePrivate", readonly filePrivate: sym_unit.Unit, }
	| { readonly tag: "tubeOrProtected", readonly tubeOrProtected: sym_unit.Unit, }
	| { readonly tag: "tubeAndProtected", readonly tubeAndProtected: sym_unit.Unit, }
	| { readonly tag: "protected", readonly protected: sym_unit.Unit, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "public", public: await sym_unit.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "tubePrivate", tubePrivate: await sym_unit.codec.read(reader)};
				}
				case 2n:
				{
					return { tag: "filePrivate", filePrivate: await sym_unit.codec.read(reader)};
				}
				case 3n:
				{
					return { tag: "tubeOrProtected", tubeOrProtected: await sym_unit.codec.read(reader)};
				}
				case 4n:
				{
					return { tag: "tubeAndProtected", tubeAndProtected: await sym_unit.codec.read(reader)};
				}
				case 5n:
				{
					return { tag: "protected", protected: await sym_unit.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "public":
				{
					const _public = value.public;
					await natCodec.write(writer, 0n);
					await sym_unit.codec.write(writer, _public);
					break;
				}
				case "tubePrivate":
				{
					const tubePrivate = value.tubePrivate;
					await natCodec.write(writer, 1n);
					await sym_unit.codec.write(writer, tubePrivate);
					break;
				}
				case "filePrivate":
				{
					const filePrivate = value.filePrivate;
					await natCodec.write(writer, 2n);
					await sym_unit.codec.write(writer, filePrivate);
					break;
				}
				case "tubeOrProtected":
				{
					const tubeOrProtected = value.tubeOrProtected;
					await natCodec.write(writer, 3n);
					await sym_unit.codec.write(writer, tubeOrProtected);
					break;
				}
				case "tubeAndProtected":
				{
					const tubeAndProtected = value.tubeAndProtected;
					await natCodec.write(writer, 4n);
					await sym_unit.codec.write(writer, tubeAndProtected);
					break;
				}
				case "protected":
				{
					const _protected = value.protected;
					await natCodec.write(writer, 5n);
					await sym_unit.codec.write(writer, _protected);
					break;
				}
			}
		},
	};
}