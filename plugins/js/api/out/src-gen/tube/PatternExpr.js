import { natCodec } from "@verilization/runtime";
import * as sym_nat from "@verilization/runtime/nat.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "binding", binding: await sym_nat.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "castBinding", castBinding: await sym_nat.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "binding":
                    {
                        const binding = value.binding;
                        await natCodec.write(writer, 0n);
                        await sym_nat.codec.write(writer, binding);
                        break;
                    }
                case "castBinding":
                    {
                        const castBinding = value.castBinding;
                        await natCodec.write(writer, 1n);
                        await sym_nat.codec.write(writer, castBinding);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=PatternExpr.js.map