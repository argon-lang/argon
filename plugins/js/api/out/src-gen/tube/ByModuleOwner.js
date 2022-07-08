import * as sym_argon_tube_ModulePath from "./ModulePath.js";
export var V1;
(function (V1) {
    function codec(Tube_codec) {
        return {
            async read(reader) {
                return { tube: await Tube_codec.read(reader), path: await sym_argon_tube_ModulePath.V1.codec.read(reader), };
            },
            async write(writer, value) {
                await Tube_codec.write(writer, value.tube);
                await sym_argon_tube_ModulePath.V1.codec.write(writer, value.path);
            },
        };
    }
    V1.codec = codec;
})(V1 || (V1 = {}));
//# sourceMappingURL=ByModuleOwner.js.map