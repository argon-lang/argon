import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_argon_tube_TraitOwner from "./TraitOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export interface V1 {
	readonly owner: sym_argon_tube_TraitOwner.V1<sym_nat.Nat>;
	readonly name: sym_option.Option<sym_argon_tube_Identifier.V1>;
	readonly signature: sym_argon_tube_ErasedSignatureParameterOnly.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { owner: await sym_argon_tube_TraitOwner.V1.codec<sym_nat.Nat>(sym_nat.codec).read(reader), name: await sym_option.codec<sym_argon_tube_Identifier.V1>(sym_argon_tube_Identifier.V1.codec).read(reader), signature: await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_TraitOwner.V1.codec<sym_nat.Nat>(sym_nat.codec).write(writer, value.owner);
			await sym_option.codec<sym_argon_tube_Identifier.V1>(sym_argon_tube_Identifier.V1.codec).write(writer, value.name);
			await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.write(writer, value.signature);
		},
	};
}
