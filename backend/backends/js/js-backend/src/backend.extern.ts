import type { DirectoryEntry } from "./backend.js";

export interface BinaryResource<_E> {
    fileName?: string | undefined;
    asBytes(): AsyncIterable<Uint8Array>;
}

export interface DirectoryResource<E> {
    fileName?: string | undefined;
    entries(): AsyncIterable<DirectoryEntry<E>>;
}

