import { natCodec } from "@verilization/runtime";
import * as sym_argon_tube_ByModuleOwner from "./ByModuleOwner.js";
export var V1;
(function (V1) {
    function codec(Tube_codec) {
        return {
            async read(reader) {
                switch (await natCodec.read(reader)) {
                    case 0n:
                        {
                            return { tag: "byModule", byModule: await sym_argon_tube_ByModuleOwner.V1.codec(Tube_codec).read(reader) };
                        }
                    default: throw new Error("Unknown tag");
                }
            },
            async write(writer, value) {
                switch (value.tag) {
                    case "byModule":
                        {
                            const byModule = value.byModule;
                            await natCodec.write(writer, 0n);
                            await sym_argon_tube_ByModuleOwner.V1.codec(Tube_codec).write(writer, byModule);
                            break;
                        }
                }
            },
        };
    }
    V1.codec = codec;
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassOwner.js.map