import { natCodec } from "@verilization/runtime";
import * as sym_argon_tube_Expression from "./Expression.js";
import * as sym_argon_tube_FieldInitializer from "./FieldInitializer.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "expression", expression: await sym_argon_tube_Expression.V1.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "fieldInitializer", fieldInitializer: await sym_argon_tube_FieldInitializer.V1.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "expression":
                    {
                        const expression = value.expression;
                        await natCodec.write(writer, 0n);
                        await sym_argon_tube_Expression.V1.codec.write(writer, expression);
                        break;
                    }
                case "fieldInitializer":
                    {
                        const fieldInitializer = value.fieldInitializer;
                        await natCodec.write(writer, 1n);
                        await sym_argon_tube_FieldInitializer.V1.codec.write(writer, fieldInitializer);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=PreInitClassConstructorStatement.js.map