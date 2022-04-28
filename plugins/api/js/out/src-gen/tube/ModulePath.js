import * as sym_list from "@verilization/runtime/list.js";
import * as sym_string from "@verilization/runtime/string.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { parts: await sym_list.codec(sym_string.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_list.codec(sym_string.codec).write(writer, value.parts);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ModulePath.js.map