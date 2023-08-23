
export interface Constructed {
    readonly type: "constructed";
    readonly constructor: string;
    readonly kwargs: ReadonlyMap<string, ESExpr>;
    readonly args: readonly ESExpr[];
}

export type ESExpr =
    | Constructed
    | boolean
    | bigint
    | string
    | Uint8Array
    | { type: "float32", value: number }
    | number
    | null;
