import * as sym_argon_tube_AccessModifier from "./AccessModifier.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_option from "@verilization/runtime/option.js";
export var V1;
(function (V1) {
    function codec(TSig_codec) {
        return {
            async read(reader) {
                return { id: await sym_nat.codec.read(reader), name: await sym_option.codec(sym_argon_tube_Identifier.V1.codec).read(reader), accessModifier: await sym_argon_tube_AccessModifier.V1.codec.read(reader), sig: await TSig_codec.read(reader), };
            },
            async write(writer, value) {
                await sym_nat.codec.write(writer, value.id);
                await sym_option.codec(sym_argon_tube_Identifier.V1.codec).write(writer, value.name);
                await sym_argon_tube_AccessModifier.V1.codec.write(writer, value.accessModifier);
                await TSig_codec.write(writer, value.sig);
            },
        };
    }
    V1.codec = codec;
})(V1 || (V1 = {}));
//# sourceMappingURL=ModuleElementDeclaration.js.map