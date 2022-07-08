import * as sym_argon_tube_TubeReference from "./TubeReference.js";
import * as sym_argon_tube_TubeType from "./TubeType.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_nat from "@verilization/runtime/nat.js";
import * as sym_string from "@verilization/runtime/string.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { name: await sym_string.codec.read(reader), moduleType: await sym_argon_tube_TubeType.V1.codec.read(reader), platforms: await sym_list.codec(sym_string.codec).read(reader), references: await sym_list.codec(sym_argon_tube_TubeReference.V1.codec).read(reader), moduleCount: await sym_nat.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_string.codec.write(writer, value.name);
            await sym_argon_tube_TubeType.V1.codec.write(writer, value.moduleType);
            await sym_list.codec(sym_string.codec).write(writer, value.platforms);
            await sym_list.codec(sym_argon_tube_TubeReference.V1.codec).write(writer, value.references);
            await sym_nat.codec.write(writer, value.moduleCount);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=Metadata.js.map