import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_argon_tube_ModuleElementDeclaration from "./ModuleElementDeclaration.js";
export interface V1 {
	readonly trait: sym_argon_tube_ModuleElementDeclaration.V1<sym_argon_tube_ErasedSignatureParameterOnly.V1>;
	readonly class: sym_argon_tube_ModuleElementDeclaration.V1<sym_argon_tube_ErasedSignatureParameterOnly.V1>;
	readonly function: sym_argon_tube_ModuleElementDeclaration.V1<sym_argon_tube_ErasedSignatureParameterOnly.V1>;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { trait: await sym_argon_tube_ModuleElementDeclaration.V1.codec<sym_argon_tube_ErasedSignatureParameterOnly.V1>(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).read(reader), class: await sym_argon_tube_ModuleElementDeclaration.V1.codec<sym_argon_tube_ErasedSignatureParameterOnly.V1>(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).read(reader), function: await sym_argon_tube_ModuleElementDeclaration.V1.codec<sym_argon_tube_ErasedSignatureParameterOnly.V1>(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_argon_tube_ModuleElementDeclaration.V1.codec<sym_argon_tube_ErasedSignatureParameterOnly.V1>(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).write(writer, value.trait);
			await sym_argon_tube_ModuleElementDeclaration.V1.codec<sym_argon_tube_ErasedSignatureParameterOnly.V1>(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).write(writer, value.class);
			await sym_argon_tube_ModuleElementDeclaration.V1.codec<sym_argon_tube_ErasedSignatureParameterOnly.V1>(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).write(writer, value.function);
		},
	};
}
