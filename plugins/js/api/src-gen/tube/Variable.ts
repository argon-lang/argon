import {Codec, FormatWriter, FormatReader, Converter, natCodec} from "@verilization/runtime";
import {RemoteObjectId, RemoteConnection, RemoteObject, InterfaceInvoker, UnknownMethodException, RPCObject, MethodInvoker, MethodResultWriter, OpaqueCodec} from "@verilization/runtime/rpc";
import * as sym_argon_tube_ParameterVariable from "./ParameterVariable.js";
import * as sym_nat from "@verilization/runtime/nat.js";
export type V1 = { readonly tag: "localVariable", readonly localVariable: sym_nat.Nat, }
	| { readonly tag: "instanceVariable", readonly instanceVariable: sym_nat.Nat, }
	| { readonly tag: "parameter", readonly parameter: sym_argon_tube_ParameterVariable.V1, };
export namespace V1 {
	export const codec: Codec<V1> = {
		async read(reader: FormatReader): Promise<V1> {
			switch(await natCodec.read(reader)) {
				case 0n:
				{
					return { tag: "localVariable", localVariable: await sym_nat.codec.read(reader)};
				}
				case 1n:
				{
					return { tag: "instanceVariable", instanceVariable: await sym_nat.codec.read(reader)};
				}
				case 2n:
				{
					return { tag: "parameter", parameter: await sym_argon_tube_ParameterVariable.V1.codec.read(reader)};
				}
				default: throw new Error("Unknown tag");			}
		},
		async write(writer: FormatWriter, value: V1): Promise<void> {
			switch(value.tag) {
				case "localVariable":
				{
					const localVariable = value.localVariable;
					await natCodec.write(writer, 0n);
					await sym_nat.codec.write(writer, localVariable);
					break;
				}
				case "instanceVariable":
				{
					const instanceVariable = value.instanceVariable;
					await natCodec.write(writer, 1n);
					await sym_nat.codec.write(writer, instanceVariable);
					break;
				}
				case "parameter":
				{
					const parameter = value.parameter;
					await natCodec.write(writer, 2n);
					await sym_argon_tube_ParameterVariable.V1.codec.write(writer, parameter);
					break;
				}
			}
		},
	};
}
