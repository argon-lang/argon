import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_string from "@verilization/runtime/string.js";
export interface V1 {
	readonly parts: sym_list.List<sym_string.String>;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { parts: await sym_list.codec<sym_string.String>(sym_string.codec).read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_list.codec<sym_string.String>(sym_string.codec).write(writer, value.parts);
		},
	};
}
