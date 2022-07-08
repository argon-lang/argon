import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_TubeReference from "./TubeReference.js";
import * as sym_argon_tube_TubeType from "./TubeType.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_string from "@verilization/runtime/string.js";
export interface V1 {
	readonly name: sym_string.String;
	readonly moduleType: sym_argon_tube_TubeType.V1;
	readonly platforms: sym_list.List<sym_string.String>;
	readonly references: sym_list.List<sym_argon_tube_TubeReference.V1>;
	readonly moduleCount: sym_nat.Nat;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { name: await sym_string.codec.read(reader), moduleType: await sym_argon_tube_TubeType.V1.codec.read(reader), platforms: await sym_list.codec<sym_string.String>(sym_string.codec).read(reader), references: await sym_list.codec<sym_argon_tube_TubeReference.V1>(sym_argon_tube_TubeReference.V1.codec).read(reader), moduleCount: await sym_nat.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_string.codec.write(writer, value.name);
			await sym_argon_tube_TubeType.V1.codec.write(writer, value.moduleType);
			await sym_list.codec<sym_string.String>(sym_string.codec).write(writer, value.platforms);
			await sym_list.codec<sym_argon_tube_TubeReference.V1>(sym_argon_tube_TubeReference.V1.codec).write(writer, value.references);
			await sym_nat.codec.write(writer, value.moduleCount);
		},
	};
}
