import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_bool from "@verilization/runtime/bool.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
	readonly varType: sym_argon_tube_Expression.V1;
	readonly name: sym_option.Option<sym_argon_tube_Identifier.V1>;
	readonly isMutable: sym_bool.Bool;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { varType: await sym_argon_tube_Expression.V1.codec.read(reader), name: await sym_option.codec<sym_argon_tube_Identifier.V1>(sym_argon_tube_Identifier.V1.codec).read(reader), isMutable: await sym_bool.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_Expression.V1.codec.write(writer, value.varType);
			await sym_option.codec<sym_argon_tube_Identifier.V1>(sym_argon_tube_Identifier.V1.codec).write(writer, value.name);
			await sym_bool.codec.write(writer, value.isMutable);
		},
	};
}
