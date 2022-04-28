import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_SigType from "./SigType.js";
export interface V1 {
	readonly argumentType: sym_argon_tube_SigType.V1;
	readonly resultType: sym_argon_tube_SigType.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { argumentType: await sym_argon_tube_SigType.V1.codec.read(reader), resultType: await sym_argon_tube_SigType.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_SigType.V1.codec.write(writer, value.argumentType);
			await sym_argon_tube_SigType.V1.codec.write(writer, value.resultType);
		},
	};
}
