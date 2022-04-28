import { natCodec } from "@verilization/runtime";
import * as sym_unit from "@verilization/runtime/unit.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "normalList", normalList: await sym_unit.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "inferrableList", inferrableList: await sym_unit.codec.read(reader) };
                    }
                case 2n:
                    {
                        return { tag: "inferrableList2", inferrableList2: await sym_unit.codec.read(reader) };
                    }
                case 3n:
                    {
                        return { tag: "requiresList", requiresList: await sym_unit.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "normalList":
                    {
                        const normalList = value.normalList;
                        await natCodec.write(writer, 0n);
                        await sym_unit.codec.write(writer, normalList);
                        break;
                    }
                case "inferrableList":
                    {
                        const inferrableList = value.inferrableList;
                        await natCodec.write(writer, 1n);
                        await sym_unit.codec.write(writer, inferrableList);
                        break;
                    }
                case "inferrableList2":
                    {
                        const inferrableList2 = value.inferrableList2;
                        await natCodec.write(writer, 2n);
                        await sym_unit.codec.write(writer, inferrableList2);
                        break;
                    }
                case "requiresList":
                    {
                        const requiresList = value.requiresList;
                        await natCodec.write(writer, 3n);
                        await sym_unit.codec.write(writer, requiresList);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ParameterListType.js.map