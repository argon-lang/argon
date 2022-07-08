import * as sym_argon_tube_ParameterVariableOwner from "./ParameterVariableOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { owner: await sym_argon_tube_ParameterVariableOwner.V1.codec.read(reader), index: await sym_nat.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_ParameterVariableOwner.V1.codec.write(writer, value.owner);
            await sym_nat.codec.write(writer, value.index);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ParameterVariable.js.map