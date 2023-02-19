
export interface BinaryResource {
    readonly resourceType: "binary";
    get fileName(): string | null;
    asAsyncIterable(): AsyncIterable<Uint8Array>;
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
}

export type FileSystemResource = BinaryResource | DirectoryResource<BinaryResource>;



export interface ResourceFactory {
    directoryResource(name: string): DirectoryResource<BinaryResource>;
    binaryResource(name: string): BinaryResource;
}

export interface ResourceRecorder {
    recordBinaryResource(resource: BinaryResource): Promise<string>;
    recordDirectoryResource(resource: DirectoryResource<BinaryResource>): Promise<string>;
}


