import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_argon_tube_FunctionBody from "./FunctionBody.js";
import * as sym_argon_tube_FunctionSignature from "./FunctionSignature.js";
import * as sym_argon_tube_MethodOwner from "./MethodOwner.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { owner: await sym_argon_tube_MethodOwner.V1.codec.read(reader), flags: await sym_nat.codec.read(reader), signature: await sym_argon_tube_FunctionSignature.V1.codec.read(reader), effects: await sym_argon_tube_EffectInfo.V1.codec.read(reader), body: await sym_option.codec(sym_argon_tube_FunctionBody.V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_MethodOwner.V1.codec.write(writer, value.owner);
            await sym_nat.codec.write(writer, value.flags);
            await sym_argon_tube_FunctionSignature.V1.codec.write(writer, value.signature);
            await sym_argon_tube_EffectInfo.V1.codec.write(writer, value.effects);
            await sym_option.codec(sym_argon_tube_FunctionBody.V1.codec).write(writer, value.body);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=MethodDefinition.js.map