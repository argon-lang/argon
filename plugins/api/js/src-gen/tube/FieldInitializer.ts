import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ExpressionWithVariables from "./ExpressionWithVariables.js";
import * as sym_string from "@verilization/runtime/string.js";
export interface V1 {
	readonly fieldName: sym_string.String;
	readonly value: sym_argon_tube_ExpressionWithVariables.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { fieldName: await sym_string.codec.read(reader), value: await sym_argon_tube_ExpressionWithVariables.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_string.codec.write(writer, value.fieldName);
			await sym_argon_tube_ExpressionWithVariables.V1.codec.write(writer, value.value);
		},
	};
}
