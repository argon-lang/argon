
export interface BinaryResource<_E> {
    fileName?: string | undefined;
    asBytes(): AsyncIterable<Uint8Array>;
}

export type TestExecutionFailure = unknown;

