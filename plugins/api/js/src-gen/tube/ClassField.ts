import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ExpressionWithVariables from "./ExpressionWithVariables.js";
import * as sym_argon_tube_Mutability from "./Mutability.js";
import * as sym_string from "@verilization/runtime/string.js";
export interface V1 {
	readonly mutability: sym_argon_tube_Mutability.V1;
	readonly name: sym_string.String;
	readonly fieldType: sym_argon_tube_ExpressionWithVariables.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { mutability: await sym_argon_tube_Mutability.V1.codec.read(reader), name: await sym_string.codec.read(reader), fieldType: await sym_argon_tube_ExpressionWithVariables.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_Mutability.V1.codec.write(writer, value.mutability);
			await sym_string.codec.write(writer, value.name);
			await sym_argon_tube_ExpressionWithVariables.V1.codec.write(writer, value.fieldType);
		},
	};
}
