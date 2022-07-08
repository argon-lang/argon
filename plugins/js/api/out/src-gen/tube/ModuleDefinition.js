import * as sym_argon_tube_ModuleExport from "./ModuleExport.js";
import * as sym_list from "@verilization/runtime/list.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { exports: await sym_list.codec(sym_argon_tube_ModuleExport.V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_list.codec(sym_argon_tube_ModuleExport.V1.codec).write(writer, value.exports);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ModuleDefinition.js.map