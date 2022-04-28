import * as sym_argon_tube_SigType from "./SigType.js";
import * as sym_list from "@verilization/runtime/list.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { arguments: await sym_list.codec(sym_argon_tube_SigType.V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_list.codec(sym_argon_tube_SigType.V1.codec).write(writer, value.arguments);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=SigTypeTuple.js.map