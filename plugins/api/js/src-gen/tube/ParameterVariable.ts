import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ParameterVariableOwner from "./ParameterVariableOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
export interface V1 {
	readonly owner: sym_argon_tube_ParameterVariableOwner.V1;
	readonly index: sym_nat.Nat;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { owner: await sym_argon_tube_ParameterVariableOwner.V1.codec.read(reader), index: await sym_nat.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_ParameterVariableOwner.V1.codec.write(writer, value.owner);
			await sym_nat.codec.write(writer, value.index);
		},
	};
}
