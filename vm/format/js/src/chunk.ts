import { TaggedValue, VMType } from "./vmtype.js";

export interface Chunk {
    readonly constants: readonly TaggedValue[];
    readonly variables: readonly VMType[];
    readonly labelTargets: readonly bigint[];
    readonly bytecode: Uint8Array;
}
