import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ClassConstructorBody from "./ClassConstructorBody.js";
import * as sym_argon_tube_ClassConstructorSignature from "./ClassConstructorSignature.js";
import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_int from "@verilization/runtime/int.js";
export interface V1 {
	readonly ownerClass: sym_int.Int;
	readonly signature: sym_argon_tube_ClassConstructorSignature.V1;
	readonly effects: sym_argon_tube_EffectInfo.V1;
	readonly body: sym_argon_tube_ClassConstructorBody.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { ownerClass: await sym_int.codec.read(reader), signature: await sym_argon_tube_ClassConstructorSignature.V1.codec.read(reader), effects: await sym_argon_tube_EffectInfo.V1.codec.read(reader), body: await sym_argon_tube_ClassConstructorBody.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_int.codec.write(writer, value.ownerClass);
			await sym_argon_tube_ClassConstructorSignature.V1.codec.write(writer, value.signature);
			await sym_argon_tube_EffectInfo.V1.codec.write(writer, value.effects);
			await sym_argon_tube_ClassConstructorBody.V1.codec.write(writer, value.body);
		},
	};
}
