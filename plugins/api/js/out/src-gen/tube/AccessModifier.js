import { natCodec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "public", public: await sym_unit.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "tubePrivate", tubePrivate: await sym_unit.codec.read(reader) };
                    }
                case 2n:
                    {
                        return { tag: "filePrivate", filePrivate: await sym_unit.codec.read(reader) };
                    }
                case 3n:
                    {
                        return { tag: "tubeOrProtected", tubeOrProtected: await sym_unit.codec.read(reader) };
                    }
                case 4n:
                    {
                        return { tag: "tubeAndProtected", tubeAndProtected: await sym_unit.codec.read(reader) };
                    }
                case 5n:
                    {
                        return { tag: "protected", protected: await sym_unit.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "public":
                    {
                        const _public = value.public;
                        await natCodec.write(writer, 0n);
                        await sym_unit.codec.write(writer, _public);
                        break;
                    }
                case "tubePrivate":
                    {
                        const tubePrivate = value.tubePrivate;
                        await natCodec.write(writer, 1n);
                        await sym_unit.codec.write(writer, tubePrivate);
                        break;
                    }
                case "filePrivate":
                    {
                        const filePrivate = value.filePrivate;
                        await natCodec.write(writer, 2n);
                        await sym_unit.codec.write(writer, filePrivate);
                        break;
                    }
                case "tubeOrProtected":
                    {
                        const tubeOrProtected = value.tubeOrProtected;
                        await natCodec.write(writer, 3n);
                        await sym_unit.codec.write(writer, tubeOrProtected);
                        break;
                    }
                case "tubeAndProtected":
                    {
                        const tubeAndProtected = value.tubeAndProtected;
                        await natCodec.write(writer, 4n);
                        await sym_unit.codec.write(writer, tubeAndProtected);
                        break;
                    }
                case "protected":
                    {
                        const _protected = value.protected;
                        await natCodec.write(writer, 5n);
                        await sym_unit.codec.write(writer, _protected);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=AccessModifier.js.map