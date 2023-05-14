import type { OptionDecodeResult } from "./options.js";
import type { Toml } from "./proto/util.js";

export interface BinaryResource {
    readonly resourceType: "binary";
    get fileName(): string | null;
    asAsyncIterable(): AsyncIterable<Uint8Array>;

    get byteSize(): (() => Promise<bigint>) | null;
}

export namespace DirectoryEntry {
    export interface Subdirectory<FileResource extends BinaryResource> {
        readonly entryType: "subdirectory";
        readonly name: string;
        readonly resource: DirectoryResource<FileResource>;
    }
    export interface File<FileResource extends BinaryResource> {
        readonly entryType: "file";
        readonly name: string;
        readonly resource: FileResource;
    }
}

export type DirectoryEntry<FileResource extends BinaryResource> = DirectoryEntry.Subdirectory<FileResource> | DirectoryEntry.File<FileResource>;

export interface DirectoryResource<FileResource extends BinaryResource> {
    resourceType: "directory";
    get fileName(): string | null;
    contents(): AsyncIterable<DirectoryEntry<FileResource>>;
    get numEntries(): (() => Promise<bigint>) | null;
}

export type FileSystemResource = BinaryResource | DirectoryResource<BinaryResource>;



export interface ResourceFactory {
    directoryResource(id: string): OptionDecodeResult<DirectoryResource<BinaryResource>>;
    binaryResource(id: string): OptionDecodeResult<BinaryResource>;
}

export interface ResourceRecorder {
    recordBinaryResource(resource: BinaryResource): Promise<string>;
    recordDirectoryResource(resource: DirectoryResource<BinaryResource>): Promise<string>;
}


