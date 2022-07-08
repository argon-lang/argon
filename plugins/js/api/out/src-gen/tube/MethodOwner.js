import { natCodec } from "@verilization/runtime";
import * as sym_int from "@verilization/runtime/int.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "byClass", byClass: await sym_int.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "byClassStatic", byClassStatic: await sym_int.codec.read(reader) };
                    }
                case 2n:
                    {
                        return { tag: "byTrait", byTrait: await sym_int.codec.read(reader) };
                    }
                case 3n:
                    {
                        return { tag: "byTraitStatic", byTraitStatic: await sym_int.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "byClass":
                    {
                        const byClass = value.byClass;
                        await natCodec.write(writer, 0n);
                        await sym_int.codec.write(writer, byClass);
                        break;
                    }
                case "byClassStatic":
                    {
                        const byClassStatic = value.byClassStatic;
                        await natCodec.write(writer, 1n);
                        await sym_int.codec.write(writer, byClassStatic);
                        break;
                    }
                case "byTrait":
                    {
                        const byTrait = value.byTrait;
                        await natCodec.write(writer, 2n);
                        await sym_int.codec.write(writer, byTrait);
                        break;
                    }
                case "byTraitStatic":
                    {
                        const byTraitStatic = value.byTraitStatic;
                        await natCodec.write(writer, 3n);
                        await sym_int.codec.write(writer, byTraitStatic);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=MethodOwner.js.map