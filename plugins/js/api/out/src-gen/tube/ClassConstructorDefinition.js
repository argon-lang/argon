import * as sym_argon_tube_ClassConstructorBody from "./ClassConstructorBody.js";
import * as sym_argon_tube_ClassConstructorSignature from "./ClassConstructorSignature.js";
import * as sym_argon_tube_EffectInfo from "./EffectInfo.js";
import * as sym_int from "@verilization/runtime/int.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { ownerClass: await sym_int.codec.read(reader), signature: await sym_argon_tube_ClassConstructorSignature.V1.codec.read(reader), effects: await sym_argon_tube_EffectInfo.V1.codec.read(reader), body: await sym_argon_tube_ClassConstructorBody.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_int.codec.write(writer, value.ownerClass);
            await sym_argon_tube_ClassConstructorSignature.V1.codec.write(writer, value.signature);
            await sym_argon_tube_EffectInfo.V1.codec.write(writer, value.effects);
            await sym_argon_tube_ClassConstructorBody.V1.codec.write(writer, value.body);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassConstructorDefinition.js.map