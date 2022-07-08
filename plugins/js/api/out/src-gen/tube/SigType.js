import * as sym_argon_tube_SigTypeClass from "./SigTypeClass.js";
import * as sym_argon_tube_SigTypeFunction from "./SigTypeFunction.js";
import * as sym_argon_tube_SigTypeTrait from "./SigTypeTrait.js";
import * as sym_argon_tube_SigTypeTuple from "./SigTypeTuple.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { erased: await sym_unit.codec.read(reader), class: await sym_argon_tube_SigTypeClass.V1.codec.read(reader), trait: await sym_argon_tube_SigTypeTrait.V1.codec.read(reader), tuple: await sym_argon_tube_SigTypeTuple.V1.codec.read(reader), function: await sym_argon_tube_SigTypeFunction.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_unit.codec.write(writer, value.erased);
            await sym_argon_tube_SigTypeClass.V1.codec.write(writer, value.class);
            await sym_argon_tube_SigTypeTrait.V1.codec.write(writer, value.trait);
            await sym_argon_tube_SigTypeTuple.V1.codec.write(writer, value.tuple);
            await sym_argon_tube_SigTypeFunction.V1.codec.write(writer, value.function);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=SigType.js.map