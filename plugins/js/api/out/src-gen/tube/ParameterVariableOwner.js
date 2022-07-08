import { natCodec } from "@verilization/runtime";
import * as sym_nat from "@verilization/runtime/nat.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "byMethod", byMethod: await sym_nat.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "byFunction", byFunction: await sym_nat.codec.read(reader) };
                    }
                case 2n:
                    {
                        return { tag: "byClass", byClass: await sym_nat.codec.read(reader) };
                    }
                case 3n:
                    {
                        return { tag: "byTrait", byTrait: await sym_nat.codec.read(reader) };
                    }
                case 4n:
                    {
                        return { tag: "byClassConstructor", byClassConstructor: await sym_nat.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "byMethod":
                    {
                        const byMethod = value.byMethod;
                        await natCodec.write(writer, 0n);
                        await sym_nat.codec.write(writer, byMethod);
                        break;
                    }
                case "byFunction":
                    {
                        const byFunction = value.byFunction;
                        await natCodec.write(writer, 1n);
                        await sym_nat.codec.write(writer, byFunction);
                        break;
                    }
                case "byClass":
                    {
                        const byClass = value.byClass;
                        await natCodec.write(writer, 2n);
                        await sym_nat.codec.write(writer, byClass);
                        break;
                    }
                case "byTrait":
                    {
                        const byTrait = value.byTrait;
                        await natCodec.write(writer, 3n);
                        await sym_nat.codec.write(writer, byTrait);
                        break;
                    }
                case "byClassConstructor":
                    {
                        const byClassConstructor = value.byClassConstructor;
                        await natCodec.write(writer, 4n);
                        await sym_nat.codec.write(writer, byClassConstructor);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ParameterVariableOwner.js.map