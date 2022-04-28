import { natCodec } from "@verilization/runtime";
import * as sym_string from "@verilization/runtime/string.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "normalName", normalName: await sym_string.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "operator", operator: await sym_string.codec.read(reader) };
                    }
                case 2n:
                    {
                        return { tag: "extension", extension: await V1.codec.read(reader) };
                    }
                case 3n:
                    {
                        return { tag: "inverse", inverse: await V1.codec.read(reader) };
                    }
                case 4n:
                    {
                        return { tag: "update", update: await V1.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "normalName":
                    {
                        const normalName = value.normalName;
                        await natCodec.write(writer, 0n);
                        await sym_string.codec.write(writer, normalName);
                        break;
                    }
                case "operator":
                    {
                        const operator = value.operator;
                        await natCodec.write(writer, 1n);
                        await sym_string.codec.write(writer, operator);
                        break;
                    }
                case "extension":
                    {
                        const extension = value.extension;
                        await natCodec.write(writer, 2n);
                        await V1.codec.write(writer, extension);
                        break;
                    }
                case "inverse":
                    {
                        const inverse = value.inverse;
                        await natCodec.write(writer, 3n);
                        await V1.codec.write(writer, inverse);
                        break;
                    }
                case "update":
                    {
                        const update = value.update;
                        await natCodec.write(writer, 4n);
                        await V1.codec.write(writer, update);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=Identifier.js.map