import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_SigTypeClass from "./SigTypeClass.js";
import * as sym_argon_tube_SigTypeFunction from "./SigTypeFunction.js";
import * as sym_argon_tube_SigTypeTrait from "./SigTypeTrait.js";
import * as sym_argon_tube_SigTypeTuple from "./SigTypeTuple.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export interface V1 {
	readonly erased: sym_unit.Unit;
	readonly class: sym_argon_tube_SigTypeClass.V1;
	readonly trait: sym_argon_tube_SigTypeTrait.V1;
	readonly tuple: sym_argon_tube_SigTypeTuple.V1;
	readonly function: sym_argon_tube_SigTypeFunction.V1;
}
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			return { erased: await sym_unit.codec.read(reader), class: await sym_argon_tube_SigTypeClass.V1.codec.read(reader), trait: await sym_argon_tube_SigTypeTrait.V1.codec.read(reader), tuple: await sym_argon_tube_SigTypeTuple.V1.codec.read(reader), function: await sym_argon_tube_SigTypeFunction.V1.codec.read(reader), };
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			await sym_unit.codec.write(writer, value.erased);
			await sym_argon_tube_SigTypeClass.V1.codec.write(writer, value.class);
			await sym_argon_tube_SigTypeTrait.V1.codec.write(writer, value.trait);
			await sym_argon_tube_SigTypeTuple.V1.codec.write(writer, value.tuple);
			await sym_argon_tube_SigTypeFunction.V1.codec.write(writer, value.function);
		},
	};
}
