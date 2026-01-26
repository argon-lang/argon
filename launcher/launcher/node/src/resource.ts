import type {
    BinaryResource, BinaryResourceSink, BinaryWriter,
    DirectoryEntry,
    DirectoryResource, DirectoryResourceSink, DirectoryWriter,
    ScopedResource,
    Stream
} from "@argon-lang/js-backend-api";
import * as path from "node:path";
import * as fs from "node:fs/promises";
import type {PromiseWithError} from "@argon-lang/noble-idl-core/util";
import type {FileHandle} from "node:fs/promises";


export class FSBinaryResource implements BinaryResource<Error> {
    constructor(path: string) {
        this.#path = path;
    }

    readonly #path: string;

    get fileName(): string | undefined {
        return path.basename(this.#path);
    }

    async* asBytes(): AsyncIterable<Uint8Array> {
        const handle = await fs.open(this.#path, "r");
        try {
            const buffer = new Uint8Array(16384);
            while(true) {
                const {bytesRead} = await handle.read(buffer);
                if(bytesRead === 0) {
                    break;
                }

                yield buffer.slice(0, bytesRead);
            }
        }
        finally {
            await handle.close();
        }
    }
}

export class FSDirectoryResource implements DirectoryResource<Error> {
    constructor(path: string) {
        this.#path = path;
    }

    readonly #path: string;


    async contents(): Promise<ScopedResource<Error, Stream<Error, DirectoryEntry<Error>>>> {
        const directoryStream = new DirectoryStream(this.#path);
        return {
            async get() {
                return directoryStream;
            },

            async close() {},
        };
    }
}

interface PendingDirectory {
    readonly path: string;
    readonly dirs: readonly string[];
}

class DirectoryStream implements Stream<Error, DirectoryEntry<Error>> {
    constructor(path: string) {
        this.#pending.push({ path, dirs: [] });
    }

    readonly #pending: PendingDirectory[] = [];

    async next(): PromiseWithError<readonly DirectoryEntry<Error>[], Error> {
        while(true) {
            const pendingDir = this.#pending.shift();
            if(pendingDir === undefined) {
                return [];
            }

            const listing = await fs.readdir(pendingDir.path, { withFileTypes: true });
            const fileEntries: DirectoryEntry<Error>[] = [];
            for(const entry of listing) {
                const entryPath = path.join(pendingDir.path, entry.name);
                if(entry.isDirectory()) {
                    this.#pending.push({
                        path: entryPath,
                        dirs: [...pendingDir.dirs, entry.name]
                    });
                }
                else {
                    fileEntries.push({
                        dirs: pendingDir.dirs,
                        fileName: entry.name,
                        resource: new FSBinaryResource(entryPath),
                    });
                }
            }

            if(fileEntries.length > 0) {
                return fileEntries;
            }
        }
    }
}

export class FSBinaryResourceSink implements BinaryResourceSink<Error> {
    constructor(path: string) {
        this.#path = path;
    }

    readonly #path: string;

    async sink(): Promise<ScopedResource<Error, BinaryWriter<Error>>> {
        const p = this.#path;
        let handle: FileHandle | undefined = undefined;
        return {
            async get() {
                if(handle === undefined) {
                    const parentDir = path.dirname(p);
                    await fs.mkdir(parentDir, {recursive: true});
                    handle = await fs.open(p, "w");
                }

                return new FSBinaryWriter(handle);
            },

            async close() {
                if(handle !== undefined) {
                    await handle.close();
                }
            }
        }
    }
}

export class FSDirectoryResourceSink implements DirectoryResourceSink<Error> {
    constructor(path: string) {
        this.#path = path;
    }

    readonly #path: string;

    async sink(): Promise<ScopedResource<Error, DirectoryWriter<Error>>> {
        let hasCreatedDir = false;
        const path = this.#path;
        const directoryWriter = new FSDirectoryWriter(path);
        return {
            async get() {
                if(!hasCreatedDir) {
                    await fs.mkdir(path, {recursive: true});
                    hasCreatedDir = true;
                }
                return directoryWriter;
            },

            async close() {
            }
        };
    }
}

class FSDirectoryWriter implements DirectoryWriter<Error> {
    constructor(path: string) {
        this.#path = path;
    }

    readonly #path: string;

    async write(dirs: readonly string[], fileName: string): Promise<ScopedResource<Error, BinaryWriter<Error>>> {
        const dirPath = path.join(this.#path, ...dirs);
        const fullPath = path.join(dirPath, fileName);

        if(!isChildPath(this.#path, fullPath)) {
            throw new Error(`Path traversal detected: ${dirPath} is not a subdirectory of ${this.#path}`);
        }

        let handle: FileHandle | undefined = undefined;
        return {
            async get() {
                if(handle === undefined) {
                    await fs.mkdir(dirPath, { recursive: true });
                    handle = await fs.open(fullPath, "w");
                }

                return new FSBinaryWriter(handle);
            },
            async close() {
                if(handle !== undefined) {
                    await handle.close();
                }
            },
        };
    }
}

function isChildPath(parent: string, child: string): boolean {
    let relative = path.relative(parent, child);
    if(relative.length === 0 || path.isAbsolute(relative)) {
        return false;
    }

    while(true) {
        let prev = relative;
        relative = path.dirname(relative);

        if(relative === "..") {
            return false;
        }

        if(relative === prev) {
            break;
        }
    }

    return true;
}



class FSBinaryWriter implements BinaryWriter<Error> {
    constructor(handle: FileHandle) {
        this.#handle = handle;
    }

    readonly #handle: FileHandle;

    async write(bytes: Uint8Array): Promise<undefined> {
        await this.#handle.write(bytes);
    }
}
