import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_ParameterListType from "./ParameterListType.js";
import * as sym_bool from "@verilization/runtime/bool.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { listType: await sym_argon_tube_ParameterListType.V1.codec.read(reader), isErased: await sym_bool.codec.read(reader), type: await sym_argon_tube_Expression.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_ParameterListType.V1.codec.write(writer, value.listType);
            await sym_bool.codec.write(writer, value.isErased);
            await sym_argon_tube_Expression.V1.codec.write(writer, value.type);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=Parameter.js.map