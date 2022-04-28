import * as sym_argon_tube_ErasedSignatureParameterOnly from "./ErasedSignatureParameterOnly.js";
import * as sym_argon_tube_ModuleElementDeclaration from "./ModuleElementDeclaration.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { trait: await sym_argon_tube_ModuleElementDeclaration.V1.codec(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).read(reader), class: await sym_argon_tube_ModuleElementDeclaration.V1.codec(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).read(reader), function: await sym_argon_tube_ModuleElementDeclaration.V1.codec(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_ModuleElementDeclaration.V1.codec(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).write(writer, value.trait);
            await sym_argon_tube_ModuleElementDeclaration.V1.codec(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).write(writer, value.class);
            await sym_argon_tube_ModuleElementDeclaration.V1.codec(sym_argon_tube_ErasedSignatureParameterOnly.V1.codec).write(writer, value.function);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ModuleExport.js.map