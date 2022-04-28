import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_Identifier from "./Identifier.js";
import * as sym_bool from "@verilization/runtime/bool.js";
import * as sym_option from "@verilization/runtime/option.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { varType: await sym_argon_tube_Expression.V1.codec.read(reader), name: await sym_option.codec(sym_argon_tube_Identifier.V1.codec).read(reader), isMutable: await sym_bool.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_Expression.V1.codec.write(writer, value.varType);
            await sym_option.codec(sym_argon_tube_Identifier.V1.codec).write(writer, value.name);
            await sym_bool.codec.write(writer, value.isMutable);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=LocalVariableDeclaration.js.map