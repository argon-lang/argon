import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_int from "@verilization/runtime/int.js";
import * as sym_list from "@verilization/runtime/list.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { baseConstructorId: await sym_int.codec.read(reader), args: await sym_list.codec(sym_argon_tube_Expression.V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_int.codec.write(writer, value.baseConstructorId);
            await sym_list.codec(sym_argon_tube_Expression.V1.codec).write(writer, value.args);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=BaseClassConstructorCall.js.map