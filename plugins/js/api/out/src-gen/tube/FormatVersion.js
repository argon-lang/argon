import * as sym_nat from "@verilization/runtime/nat.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { major: await sym_nat.codec.read(reader), minor: await sym_nat.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_nat.codec.write(writer, value.major);
            await sym_nat.codec.write(writer, value.minor);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=FormatVersion.js.map