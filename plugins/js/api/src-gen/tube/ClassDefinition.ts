import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ClassField from "./ClassField.js";
import * as sym_argon_tube_ClassOwner from "./ClassOwner.js";
import * as sym_argon_tube_ClassSignature from "./ClassSignature.js";
import * as sym_argon_tube_ConstructorMember from "./ConstructorMember.js";
import * as sym_argon_tube_MethodMember from "./MethodMember.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export interface V1 {
	readonly owner: sym_argon_tube_ClassOwner.V1<sym_unit.Unit>;
	readonly flags: sym_nat.Nat;
	readonly signature: sym_argon_tube_ClassSignature.V1;
	readonly fields: sym_list.List<sym_argon_tube_ClassField.V1>;
	readonly methods: sym_list.List<sym_argon_tube_MethodMember.V1>;
	readonly staticMethods: sym_list.List<sym_argon_tube_MethodMember.V1>;
	readonly constructors: sym_list.List<sym_argon_tube_ConstructorMember.V1>;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { owner: await sym_argon_tube_ClassOwner.V1.codec<sym_unit.Unit>(sym_unit.codec).read(reader), flags: await sym_nat.codec.read(reader), signature: await sym_argon_tube_ClassSignature.V1.codec.read(reader), fields: await sym_list.codec<sym_argon_tube_ClassField.V1>(sym_argon_tube_ClassField.V1.codec).read(reader), methods: await sym_list.codec<sym_argon_tube_MethodMember.V1>(sym_argon_tube_MethodMember.V1.codec).read(reader), staticMethods: await sym_list.codec<sym_argon_tube_MethodMember.V1>(sym_argon_tube_MethodMember.V1.codec).read(reader), constructors: await sym_list.codec<sym_argon_tube_ConstructorMember.V1>(sym_argon_tube_ConstructorMember.V1.codec).read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_ClassOwner.V1.codec<sym_unit.Unit>(sym_unit.codec).write(writer, value.owner);
			await sym_nat.codec.write(writer, value.flags);
			await sym_argon_tube_ClassSignature.V1.codec.write(writer, value.signature);
			await sym_list.codec<sym_argon_tube_ClassField.V1>(sym_argon_tube_ClassField.V1.codec).write(writer, value.fields);
			await sym_list.codec<sym_argon_tube_MethodMember.V1>(sym_argon_tube_MethodMember.V1.codec).write(writer, value.methods);
			await sym_list.codec<sym_argon_tube_MethodMember.V1>(sym_argon_tube_MethodMember.V1.codec).write(writer, value.staticMethods);
			await sym_list.codec<sym_argon_tube_ConstructorMember.V1>(sym_argon_tube_ConstructorMember.V1.codec).write(writer, value.constructors);
		},
	};
}