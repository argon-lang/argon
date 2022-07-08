import * as sym_argon_tube_ErasedSignature from "./ErasedSignature.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_argon_tube_MethodOwner from "./MethodOwner.js";
import * as sym_option from "@verilization/runtime/option.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { owner: await sym_argon_tube_MethodOwner.V1.codec.read(reader), name: await sym_option.codec(sym_argon_tube_Identifier.V1.codec).read(reader), signature: await sym_argon_tube_ErasedSignature.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_MethodOwner.V1.codec.write(writer, value.owner);
            await sym_option.codec(sym_argon_tube_Identifier.V1.codec).write(writer, value.name);
            await sym_argon_tube_ErasedSignature.V1.codec.write(writer, value.signature);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=MethodReference.js.map