import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_ExpressionConstructor from "./ExpressionConstructor.js";
import * as sym_list from "@verilization/runtime/list.js";
export interface V1 {
	readonly constructor: sym_argon_tube_ExpressionConstructor.V1;
	readonly args: sym_list.List<V1>;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { constructor: await sym_argon_tube_ExpressionConstructor.V1.codec.read(reader), args: await sym_list.codec<V1>(V1.codec).read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_ExpressionConstructor.V1.codec.write(writer, value.constructor);
			await sym_list.codec<V1>(V1.codec).write(writer, value.args);
		},
	};
}
