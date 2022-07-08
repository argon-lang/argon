import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_int from "@verilization/runtime/int.js";
export interface V1 {
	readonly ownerClass: sym_int.Int;
	readonly signature: sym_argon_tube_ErasedSignatureParameterOnly.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { ownerClass: await sym_int.codec.read(reader), signature: await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_int.codec.write(writer, value.ownerClass);
			await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.write(writer, value.signature);
		},
	};
}
