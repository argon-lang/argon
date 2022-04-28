import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_argon_tube_FunctionBody from "./FunctionBody.js";
import * as sym_argon_tube_FunctionSignature from "./FunctionSignature.js";
import * as sym_argon_tube_MethodOwner from "./MethodOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
	readonly owner: sym_argon_tube_MethodOwner.V1;
	readonly flags: sym_nat.Nat;
	readonly signature: sym_argon_tube_FunctionSignature.V1;
	readonly effects: sym_argon_tube_EffectInfo.V1;
	readonly body: sym_option.Option<sym_argon_tube_FunctionBody.V1>;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { owner: await sym_argon_tube_MethodOwner.V1.codec.read(reader), flags: await sym_nat.codec.read(reader), signature: await sym_argon_tube_FunctionSignature.V1.codec.read(reader), effects: await sym_argon_tube_EffectInfo.V1.codec.read(reader), body: await sym_option.codec<sym_argon_tube_FunctionBody.V1>(sym_argon_tube_FunctionBody.V1.codec).read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_MethodOwner.V1.codec.write(writer, value.owner);
			await sym_nat.codec.write(writer, value.flags);
			await sym_argon_tube_FunctionSignature.V1.codec.write(writer, value.signature);
			await sym_argon_tube_EffectInfo.V1.codec.write(writer, value.effects);
			await sym_option.codec<sym_argon_tube_FunctionBody.V1>(sym_argon_tube_FunctionBody.V1.codec).write(writer, value.body);
		},
	};
}
