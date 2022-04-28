import { natCodec } from "@verilization/runtime";
import * as sym_bool from "@verilization/runtime/bool.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "isPure", isPure: await sym_bool.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "isPure":
                    {
                        const isPure = value.isPure;
                        await natCodec.write(writer, 0n);
                        await sym_bool.codec.write(writer, isPure);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=EffectInfo.js.map