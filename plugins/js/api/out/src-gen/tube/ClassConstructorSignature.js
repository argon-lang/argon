import * as sym_argon_tube_LocalVariableDeclaration from "./LocalVariableDeclaration.js";
import * as sym_argon_tube_Parameter from "./Parameter.js";
import * as sym_list from "@verilization/runtime/list.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { variables: await sym_list.codec(sym_argon_tube_LocalVariableDeclaration.V1.codec).read(reader), parameters: await sym_list.codec(sym_argon_tube_Parameter.V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_list.codec(sym_argon_tube_LocalVariableDeclaration.V1.codec).write(writer, value.variables);
            await sym_list.codec(sym_argon_tube_Parameter.V1.codec).write(writer, value.parameters);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassConstructorSignature.js.map