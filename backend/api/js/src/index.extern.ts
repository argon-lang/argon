
export interface BinaryResource<_E> {
    readonly fileName?: string | undefined;
    asBytes(): AsyncIterable<Uint8Array>;
}

export type TestExecutionFailure = unknown;

