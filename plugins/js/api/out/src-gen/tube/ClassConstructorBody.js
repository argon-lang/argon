import { natCodec } from "@verilization/runtime";
import * as sym_argon_tube_ClassConstructorExpressionBody from "./ClassConstructorExpressionBody.js";
import * as sym_unit from "@verilization/runtime/unit.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            switch (await natCodec.read(reader)) {
                case 0n:
                    {
                        return { tag: "expressionBody", expressionBody: await sym_argon_tube_ClassConstructorExpressionBody.V1.codec.read(reader) };
                    }
                case 1n:
                    {
                        return { tag: "externalImplementation", externalImplementation: await sym_unit.codec.read(reader) };
                    }
                default: throw new Error("Unknown tag");
            }
        },
        async write(writer, value) {
            switch (value.tag) {
                case "expressionBody":
                    {
                        const expressionBody = value.expressionBody;
                        await natCodec.write(writer, 0n);
                        await sym_argon_tube_ClassConstructorExpressionBody.V1.codec.write(writer, expressionBody);
                        break;
                    }
                case "externalImplementation":
                    {
                        const externalImplementation = value.externalImplementation;
                        await natCodec.write(writer, 1n);
                        await sym_unit.codec.write(writer, externalImplementation);
                        break;
                    }
            }
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassConstructorBody.js.map