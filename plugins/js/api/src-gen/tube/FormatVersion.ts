import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_nat from "@verilization/runtime/nat.js";
export interface V1 {
	readonly major: sym_nat.Nat;
	readonly minor: sym_nat.Nat;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { major: await sym_nat.codec.read(reader), minor: await sym_nat.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_nat.codec.write(writer, value.major);
			await sym_nat.codec.write(writer, value.minor);
		},
	};
}
