import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_ParameterListType from "./ParameterListType.js";
import * as sym_bool from "@verilization/runtime/bool.js";
export interface V1 {
	readonly listType: sym_argon_tube_ParameterListType.V1;
	readonly isErased: sym_bool.Bool;
	readonly type: sym_argon_tube_Expression.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { listType: await sym_argon_tube_ParameterListType.V1.codec.read(reader), isErased: await sym_bool.codec.read(reader), type: await sym_argon_tube_Expression.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_ParameterListType.V1.codec.write(writer, value.listType);
			await sym_bool.codec.write(writer, value.isErased);
			await sym_argon_tube_Expression.V1.codec.write(writer, value.type);
		},
	};
}
