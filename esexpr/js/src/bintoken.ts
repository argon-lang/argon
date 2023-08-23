
type WithIntegerType = "constructor" | "int" | "neg_int" | "string" | "binary" | "kwarg";
type Fixed = "null" | "constructor_end" | "true" | "false" | "float32" | "float64";

export type BinToken =
    | { type: WithIntegerType, value: bigint }
    | { type: Fixed };
