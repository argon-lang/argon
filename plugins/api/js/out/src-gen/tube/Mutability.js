import { natCodec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "mutable", mutable: await sym_unit.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "nonMutable", nonMutable: await sym_unit.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "mutable":
                    {
                        const mutable = value.mutable;
                        await natCodec.write(writer, 0n);
                        await sym_unit.codec.write(writer, mutable);
                        break;
                    }
                case "nonMutable":
                    {
                        const nonMutable = value.nonMutable;
                        await natCodec.write(writer, 1n);
                        await sym_unit.codec.write(writer, nonMutable);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=Mutability.js.map