import type * as tube from "@argon-lang/plugin-api/tube";
import type { ExternImpl } from "../extern.js";
import type { JSOptions } from "../options.js";

export abstract class EmitTubeCommon {
    constructor(
        public readonly options: JSOptions,
        public readonly emitTubeInfo: tube.EmitTubeInfo<ExternImpl, ExternImpl, ExternImpl>,
        public readonly metadata: tube.Metadata,
    ) {}

    get tube(): tube.SerializedTube {
        return this.emitTubeInfo.tube;
    }
    
}
