export * from "./js-platform-metadata.js";

export interface InputFile {
    open(): Promise<InputStream>;
    get fileName(): string;
}

export interface InputStream {
    read(buffer: Uint8Array): Promise<number>;
    close(): Promise<void>;
}

export interface OutputDirectory {
    getFile(...path: readonly [...readonly string[], string]): OutputFile;
}

export interface OutputFile {
    open(): Promise<OutputStream>;
    delete(): Promise<void>;
}

export interface OutputStream {
    write(buffer: Uint8Array): Promise<void>;
    close(): Promise<void>;
}

export interface JSPlatformMetadataOptions {
    readonly packageName?: string | undefined;
    readonly externFiles: readonly InputFile[];
}

export interface JSCodeGenOptions {
    readonly tube: InputFile;
    readonly outputDirectory: OutputDirectory;
    readonly executable?: string | undefined;
}

