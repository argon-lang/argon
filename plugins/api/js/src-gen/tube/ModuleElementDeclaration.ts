import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_AccessModifier from "./AccessModifier.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1<TSig> {
	readonly id: sym_nat.Nat;
	readonly name: sym_option.Option<sym_argon_tube_Identifier.V1>;
	readonly accessModifier: sym_argon_tube_AccessModifier.V1;
	readonly sig: TSig;
}
export namespace V1 {
	export function codec<TSig>(TSig_codec: Codec<TSig>): Codec<V1<TSig>> {
		return {
			async read(reader: FormatReader): Promise<V1<TSig>> {
				return { id: await sym_nat.codec.read(reader), name: await sym_option.codec<sym_argon_tube_Identifier.V1>(sym_argon_tube_Identifier.V1.codec).read(reader), accessModifier: await sym_argon_tube_AccessModifier.V1.codec.read(reader), sig: await TSig_codec.read(reader), };
			},
			async write(writer: FormatWriter, value: V1<TSig>): Promise<void> {
				await sym_nat.codec.write(writer, value.id);
				await sym_option.codec<sym_argon_tube_Identifier.V1>(sym_argon_tube_Identifier.V1.codec).write(writer, value.name);
				await sym_argon_tube_AccessModifier.V1.codec.write(writer, value.accessModifier);
				await TSig_codec.write(writer, value.sig);
			},
		};
	}
}
