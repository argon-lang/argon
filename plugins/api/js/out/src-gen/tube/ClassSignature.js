import * as sym_argon_tube_ClassType from "./ClassType.js";
import * as sym_argon_tube_LocalVariableDeclaration from "./LocalVariableDeclaration.js";
import * as sym_argon_tube_Parameter from "./Parameter.js";
import * as sym_argon_tube_TraitType from "./TraitType.js";
import * as sym_list from "@verilization/runtime/list.js";
import * as sym_option from "@verilization/runtime/option.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { variables: await sym_list.codec(sym_argon_tube_LocalVariableDeclaration.V1.codec).read(reader), parameters: await sym_list.codec(sym_argon_tube_Parameter.V1.codec).read(reader), baseClass: await sym_option.codec(sym_argon_tube_ClassType.V1.codec).read(reader), baseTraits: await sym_list.codec(sym_argon_tube_TraitType.V1.codec).read(reader), };
        },
        async write(writer, value) {
            await sym_list.codec(sym_argon_tube_LocalVariableDeclaration.V1.codec).write(writer, value.variables);
            await sym_list.codec(sym_argon_tube_Parameter.V1.codec).write(writer, value.parameters);
            await sym_option.codec(sym_argon_tube_ClassType.V1.codec).write(writer, value.baseClass);
            await sym_list.codec(sym_argon_tube_TraitType.V1.codec).write(writer, value.baseTraits);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ClassSignature.js.map