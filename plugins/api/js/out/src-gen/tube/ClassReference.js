import * as sym_argon_tube_ClassOwner from "./ClassOwner.js";
import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { owner: await sym_argon_tube_ClassOwner.V1.codec(sym_nat.codec).read(reader), name: await sym_option.codec(sym_argon_tube_Identifier.V1.codec).read(reader), signature: await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_ClassOwner.V1.codec(sym_nat.codec).write(writer, value.owner);
            await sym_option.codec(sym_argon_tube_Identifier.V1.codec).write(writer, value.name);
            await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.write(writer, value.signature);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassReference.js.map