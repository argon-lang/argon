import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_LocalVariableDeclaration from "./LocalVariableDeclaration.js";
import * as sym_argon_tube_Parameter from "./Parameter.js";
import * as sym_list from "@verilization/runtime/list.js";
export interface V1 {
	readonly variables: sym_list.List<sym_argon_tube_LocalVariableDeclaration.V1>;
	readonly parameters: sym_list.List<sym_argon_tube_Parameter.V1>;
	readonly returnType: sym_argon_tube_Expression.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { variables: await sym_list.codec<sym_argon_tube_LocalVariableDeclaration.V1>(sym_argon_tube_LocalVariableDeclaration.V1.codec).read(reader), parameters: await sym_list.codec<sym_argon_tube_Parameter.V1>(sym_argon_tube_Parameter.V1.codec).read(reader), returnType: await sym_argon_tube_Expression.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_list.codec<sym_argon_tube_LocalVariableDeclaration.V1>(sym_argon_tube_LocalVariableDeclaration.V1.codec).write(writer, value.variables);
			await sym_list.codec<sym_argon_tube_Parameter.V1>(sym_argon_tube_Parameter.V1.codec).write(writer, value.parameters);
			await sym_argon_tube_Expression.V1.codec.write(writer, value.returnType);
		},
	};
}
