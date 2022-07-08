import * as sym_argon_tube_AccessModifier from "./AccessModifier.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_int from "@verilization/runtime/int.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { id: await sym_int.codec.read(reader), name: await sym_argon_tube_Identifier.V1.codec.read(reader), accessModifier: await sym_argon_tube_AccessModifier.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_int.codec.write(writer, value.id);
            await sym_argon_tube_Identifier.V1.codec.write(writer, value.name);
            await sym_argon_tube_AccessModifier.V1.codec.write(writer, value.accessModifier);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=MethodMember.js.map