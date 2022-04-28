import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_int from "@verilization/runtime/int.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { ownerClass: await sym_int.codec.read(reader), signature: await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_int.codec.write(writer, value.ownerClass);
            await sym_argon_tube_ErasedSignatureParameterOnly.V1.codec.write(writer, value.signature);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassConstructorReference.js.map