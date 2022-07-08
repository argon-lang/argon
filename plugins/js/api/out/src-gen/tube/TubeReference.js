import * as sym_string from "@verilization/runtime/string.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { name: await sym_string.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_string.codec.write(writer, value.name);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=TubeReference.js.map