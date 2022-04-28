import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_LocalVariableDeclaration from "./LocalVariableDeclaration.js";
import * as sym_list from "@verilization/runtime/list.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { variables: await sym_list.codec(sym_argon_tube_LocalVariableDeclaration.V1.codec).read(reader), expression: await sym_argon_tube_Expression.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_list.codec(sym_argon_tube_LocalVariableDeclaration.V1.codec).write(writer, value.variables);
            await sym_argon_tube_Expression.V1.codec.write(writer, value.expression);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ExpressionWithVariables.js.map