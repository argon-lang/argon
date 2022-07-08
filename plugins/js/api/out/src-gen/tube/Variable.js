import { natCodec } from "@verilization/runtime";
import * as sym_argon_tube_ParameterVariable from "./ParameterVariable.js";
import * as sym_nat from "@verilization/runtime/nat.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "localVariable", localVariable: await sym_nat.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "instanceVariable", instanceVariable: await sym_nat.codec.read(reader) };
                    }
                case 2n:
                    {
                        return { tag: "parameter", parameter: await sym_argon_tube_ParameterVariable.V1.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
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
})(V1 || (V1 = {}));
//# sourceMappingURL=Variable.js.map