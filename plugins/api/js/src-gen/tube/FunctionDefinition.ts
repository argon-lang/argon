import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_argon_tube_FunctionBody from "./FunctionBody.js";
import * as sym_argon_tube_FunctionOwner from "./FunctionOwner.js";
import * as sym_argon_tube_FunctionSignature from "./FunctionSignature.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export interface V1 {
	readonly owner: sym_argon_tube_FunctionOwner.V1<sym_unit.Unit>;
	readonly signature: sym_argon_tube_FunctionSignature.V1;
	readonly effects: sym_argon_tube_EffectInfo.V1;
	readonly body: sym_argon_tube_FunctionBody.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { owner: await sym_argon_tube_FunctionOwner.V1.codec<sym_unit.Unit>(sym_unit.codec).read(reader), signature: await sym_argon_tube_FunctionSignature.V1.codec.read(reader), effects: await sym_argon_tube_EffectInfo.V1.codec.read(reader), body: await sym_argon_tube_FunctionBody.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_FunctionOwner.V1.codec<sym_unit.Unit>(sym_unit.codec).write(writer, value.owner);
			await sym_argon_tube_FunctionSignature.V1.codec.write(writer, value.signature);
			await sym_argon_tube_EffectInfo.V1.codec.write(writer, value.effects);
			await sym_argon_tube_FunctionBody.V1.codec.write(writer, value.body);
		},
	};
}
