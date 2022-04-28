import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_AccessModifier from "./AccessModifier.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_int from "@verilization/runtime/int.js";
export interface V1 {
	readonly id: sym_int.Int;
	readonly name: sym_argon_tube_Identifier.V1;
	readonly accessModifier: sym_argon_tube_AccessModifier.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { id: await sym_int.codec.read(reader), name: await sym_argon_tube_Identifier.V1.codec.read(reader), accessModifier: await sym_argon_tube_AccessModifier.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_int.codec.write(writer, value.id);
			await sym_argon_tube_Identifier.V1.codec.write(writer, value.name);
			await sym_argon_tube_AccessModifier.V1.codec.write(writer, value.accessModifier);
		},
	};
}
