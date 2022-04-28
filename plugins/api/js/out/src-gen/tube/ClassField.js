import * as sym_argon_tube_ExpressionWithVariables from "./ExpressionWithVariables.js";
import * as sym_argon_tube_Mutability from "./Mutability.js";
import * as sym_string from "@verilization/runtime/string.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { mutability: await sym_argon_tube_Mutability.V1.codec.read(reader), name: await sym_string.codec.read(reader), fieldType: await sym_argon_tube_ExpressionWithVariables.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_Mutability.V1.codec.write(writer, value.mutability);
            await sym_string.codec.write(writer, value.name);
            await sym_argon_tube_ExpressionWithVariables.V1.codec.write(writer, value.fieldType);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassField.js.map