import * as sym_argon_tube_ExpressionWithVariables from "./ExpressionWithVariables.js";
import * as sym_string from "@verilization/runtime/string.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { fieldName: await sym_string.codec.read(reader), value: await sym_argon_tube_ExpressionWithVariables.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_string.codec.write(writer, value.fieldName);
            await sym_argon_tube_ExpressionWithVariables.V1.codec.write(writer, value.value);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=FieldInitializer.js.map