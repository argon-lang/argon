import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ModulePath from "./ModulePath.js";
export interface V1<Tube> {
	readonly tube: Tube;
	readonly path: sym_argon_tube_ModulePath.V1;
}
export namespace V1 {
	export function codec<Tube>(Tube_codec: Codec<Tube>): Codec<V1<Tube>> {
		return {
			async read(reader: FormatReader): Promise<V1<Tube>> {
				return { tube: await Tube_codec.read(reader), path: await sym_argon_tube_ModulePath.V1.codec.read(reader), };
			},
			async write(writer: FormatWriter, value: V1<Tube>): Promise<void> {
				await Tube_codec.write(writer, value.tube);
				await sym_argon_tube_ModulePath.V1.codec.write(writer, value.path);
			},
		};
	}
}
