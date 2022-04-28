import { natCodec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "interface", interface: await sym_unit.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "definition", definition: await sym_unit.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "interface":
                    {
                        const _interface = value.interface;
                        await natCodec.write(writer, 0n);
                        await sym_unit.codec.write(writer, _interface);
                        break;
                    }
                case "definition":
                    {
                        const definition = value.definition;
                        await natCodec.write(writer, 1n);
                        await sym_unit.codec.write(writer, definition);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=TubeType.js.map