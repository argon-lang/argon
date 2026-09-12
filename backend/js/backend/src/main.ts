#!/usr/bin/env node

import { mkdir, open, rm, type FileHandle } from "node:fs/promises";
import { basename, dirname, isAbsolute, relative, resolve } from "node:path";
import { Command } from "@commander-js/extra-typings";
import { writeExprs } from "@argon-lang/esexpr/binary_format";
import type { InputFile, InputStream, OutputDirectory, OutputFile, OutputStream } from "@argon-lang/js-backend-api";
import { PlatformMetadataResult } from "@argon-lang/js-backend-api/metadata.js";
import { TaskMessage } from "@argon-lang/js-backend-api/task-messages.js";
import { loadMetadata, codegen } from "./index.js";


class LocalInputFile implements InputFile {
    constructor(private readonly path: string) {}

    get fileName(): string {
        return basename(this.path);
    }

    async open(): Promise<InputStream> {
        return new LocalInputStream(await open(this.path, "r"));
    }
}

class LocalInputStream implements InputStream {
    constructor(private readonly file: FileHandle) {}

    async read(buffer: Uint8Array): Promise<number> {
        const result = await this.file.read(buffer);
        return result.bytesRead;
    }

    async close(): Promise<void> {
        await this.file.close();
    }
}

export class LocalOutputDirectory implements OutputDirectory {
    private readonly rootPath: string;

    constructor(path: string) {
        this.rootPath = resolve(path);
    }

    getFile(...path: readonly [...readonly string[], string]): OutputFile {
        return new LocalOutputFile(this.resolveOutputPath(path));
    }

    private resolveOutputPath(path: readonly string[]): string {
        const filePath = resolve(this.rootPath, ...path);
        const relativePath = relative(this.rootPath, filePath);

        if(relativePath === "" || relativePath.startsWith("..") || isAbsolute(relativePath)) {
            throw new Error("Output file path must be inside the output directory");
        }

        return filePath;
    }
}

class LocalOutputFile implements OutputFile {
    constructor(private readonly path: string) {}

    async open(): Promise<OutputStream> {
        await mkdir(dirname(this.path), { recursive: true });
        return new LocalOutputStream(await open(this.path, "w"));
    }

    async delete(): Promise<void> {
        await rm(this.path, { force: true });
    }
}

class LocalOutputStream implements OutputStream {
    constructor(private readonly file: FileHandle) {}

    async write(buffer: Uint8Array): Promise<void> {
        await this.file.write(buffer);
    }

    async close(): Promise<void> {
        await this.file.close();
    }
}

async function platformMetadataJs(
    options: {
        readonly packageName?: string | undefined;
        readonly extern: readonly string[];
        readonly outputFile: string;
    },
): Promise<void> {
    const metadata = await loadMetadata({
        packageName: options.packageName,
        externFiles: options.extern.map(file => new LocalInputFile(file)),
    });

    await writePlatformMetadata(metadata, options.outputFile);
}

async function writePlatformMetadata(metadata: PlatformMetadataResult, outputFile: string): Promise<void> {
    const expr = PlatformMetadataResult.codec.encode({
        ...metadata,
        platform: "js",
    });
    const file = await open(outputFile, "w");

    try {
        for await(const chunk of writeExprs([expr])) {
            await file.write(chunk);
        }
    }
    finally {
        await file.close();
    }
}

function collect(value: string, previous: readonly string[]): readonly string[] {
    return [...previous, value];
}

async function codegenJs(
    options: {
        readonly input: string;
        readonly output: string;
        readonly executable?: string | undefined;
    },
): Promise<void> {
    await codegen({
        tube: new LocalInputFile(options.input),
        outputDirectory: new LocalOutputDirectory(options.output),
        executable: options.executable,
    })
}

const program = new Command();

program
    .name("argon-js-backend")
    .description("Argon JavaScript backend CLI")
    .option("--output-format <format>", "Task message output format (text or esexpr)", value => {
        if(value !== "text" && value !== "esexpr") {
            throw new Error(`Invalid output format: ${value}`);
        }
        return value;
    }, "text");

program
    .command("platform-metadata")
    .description("Load platform-specific metadata")
    .command("js")
    .description("Load platform metadata for JavaScript")
    .option("--package-name <name>", "NPM package name for the generated tube")
    .option("--extern <file>", "JavaScript extern file", collect, [])
    .requiredOption("-o, --output-file <file>", "Output platform metadata file")
    .action(platformMetadataJs);

program
    .command("codegen")
    .description("Generate code from Argon VM IR")
    .command("js")
    .description("Generate JavaScript code from Argon VM IR")
    .requiredOption("-i, --input <file>", "Input Argon VM IR file")
    .requiredOption("-o, --output <dir>", "Output directory")
    .option("--executable <name>", "Name of the executable to generate")
    .action(codegenJs);

try {
    await program.parseAsync(process.argv);
}
catch(err) {
    const message = err instanceof Error ? err.message : String(err);
    const taskMessage = `argon-js-backend: ${message}`;
    if((program.opts() as { outputFormat: string }).outputFormat === "esexpr") {
        const expr = TaskMessage.codec.encode({ $type: "task-error", message: taskMessage });
        for await(const chunk of writeExprs([expr])) {
            process.stdout.write(chunk);
        }
    }
    else {
        console.error(taskMessage);
        if(err instanceof Error && err.stack) {
            console.error(err.stack);
        }
    }
    process.exitCode = 1;
}
