import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_argon_tube_FunctionBody from "./FunctionBody.js";
import * as sym_argon_tube_FunctionOwner from "./FunctionOwner.js";
import * as sym_argon_tube_FunctionSignature from "./FunctionSignature.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { owner: await sym_argon_tube_FunctionOwner.V1.codec(sym_unit.codec).read(reader), signature: await sym_argon_tube_FunctionSignature.V1.codec.read(reader), effects: await sym_argon_tube_EffectInfo.V1.codec.read(reader), body: await sym_argon_tube_FunctionBody.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_FunctionOwner.V1.codec(sym_unit.codec).write(writer, value.owner);
            await sym_argon_tube_FunctionSignature.V1.codec.write(writer, value.signature);
            await sym_argon_tube_EffectInfo.V1.codec.write(writer, value.effects);
            await sym_argon_tube_FunctionBody.V1.codec.write(writer, value.body);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=FunctionDefinition.js.map