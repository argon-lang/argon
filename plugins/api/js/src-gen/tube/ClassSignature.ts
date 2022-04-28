import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ClassType from "./ClassType.js";
import * as sym_argon_tube_LocalVariableDeclaration from "./LocalVariableDeclaration.js";
import * as sym_argon_tube_Parameter from "./Parameter.js";
import * as sym_argon_tube_TraitType from "./TraitType.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
	readonly variables: sym_list.List<sym_argon_tube_LocalVariableDeclaration.V1>;
	readonly parameters: sym_list.List<sym_argon_tube_Parameter.V1>;
	readonly baseClass: sym_option.Option<sym_argon_tube_ClassType.V1>;
	readonly baseTraits: sym_list.List<sym_argon_tube_TraitType.V1>;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { variables: await sym_list.codec<sym_argon_tube_LocalVariableDeclaration.V1>(sym_argon_tube_LocalVariableDeclaration.V1.codec).read(reader), parameters: await sym_list.codec<sym_argon_tube_Parameter.V1>(sym_argon_tube_Parameter.V1.codec).read(reader), baseClass: await sym_option.codec<sym_argon_tube_ClassType.V1>(sym_argon_tube_ClassType.V1.codec).read(reader), baseTraits: await sym_list.codec<sym_argon_tube_TraitType.V1>(sym_argon_tube_TraitType.V1.codec).read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_list.codec<sym_argon_tube_LocalVariableDeclaration.V1>(sym_argon_tube_LocalVariableDeclaration.V1.codec).write(writer, value.variables);
			await sym_list.codec<sym_argon_tube_Parameter.V1>(sym_argon_tube_Parameter.V1.codec).write(writer, value.parameters);
			await sym_option.codec<sym_argon_tube_ClassType.V1>(sym_argon_tube_ClassType.V1.codec).write(writer, value.baseClass);
			await sym_list.codec<sym_argon_tube_TraitType.V1>(sym_argon_tube_TraitType.V1.codec).write(writer, value.baseTraits);
		},
	};
}
