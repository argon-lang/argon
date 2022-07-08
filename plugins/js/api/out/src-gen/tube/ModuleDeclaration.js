import * as sym_argon_tube_ModulePath from "./ModulePath.js";
export var V1;
(function (V1) {
    V1.codec = {
        async read(reader) {
            return { path: await sym_argon_tube_ModulePath.V1.codec.read(reader), };
        },
        async write(writer, value) {
            await sym_argon_tube_ModulePath.V1.codec.write(writer, value.path);
        },
    };
})(V1 || (V1 = {}));
//# sourceMappingURL=ModuleDeclaration.js.map