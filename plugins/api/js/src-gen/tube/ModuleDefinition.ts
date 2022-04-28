import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ModuleExport from "./ModuleExport.js";
import * as sym_list from "@verilization/runtime/list.js";
export interface V1 {
	readonly exports: sym_list.List<sym_argon_tube_ModuleExport.V1>;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { exports: await sym_list.codec<sym_argon_tube_ModuleExport.V1>(sym_argon_tube_ModuleExport.V1.codec).read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_list.codec<sym_argon_tube_ModuleExport.V1>(sym_argon_tube_ModuleExport.V1.codec).write(writer, value.exports);
		},
	};
}
