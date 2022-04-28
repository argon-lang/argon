import * as sym_argon_tube_ExpressionConstructor from "./ExpressionConstructor.js";
import * as sym_list from "@verilization/runtime/list.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { constructor: await sym_argon_tube_ExpressionConstructor.V1.codec.read(reader), args: await sym_list.codec(V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_ExpressionConstructor.V1.codec.write(writer, value.constructor);
            await sym_list.codec(V1.codec).write(writer, value.args);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=Expression.js.map