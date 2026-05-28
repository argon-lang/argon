#!/usr/bin/env node

import { open, type FileHandle } from "node:fs/promises";
import { basename } from "node:path";
import { Command, type CommandUnknownOpts } from "@commander-js/extra-typings";
import { writeExprs } from "@argon-lang/esexpr/binary_format";
import type { InputFile, InputStream, JSPlatformMetadata } from "@argon-lang/js-backend-api";
import { PlatformMetadataResult } from "@argon-lang/js-backend-api/metadata.js";
import { loadMetadata } from "./index.js";


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

async function platformMetadataJs(
    options: {
        readonly packageName?: string | undefined;
        readonly externFile: readonly string[];
        readonly outputFile: string;
    },
): Promise<void> {
    const metadata = await loadMetadata({
        packageName: options.packageName,
        externFiles: options.externFile.map(file => new LocalInputFile(file)),
    });

    await writePlatformMetadata(metadata, options.outputFile);
}

async function writePlatformMetadata(metadata: JSPlatformMetadata, outputFile: string): Promise<void> {
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

const program = new Command();

program
    .name("argon-js-backend")
    .description("Argon JavaScript backend CLI");

function printHelpDescription(commandPath: readonly string[]): void {
    let current: CommandUnknownOpts = program;

    for(const commandName of commandPath) {
        const next = current.commands.find(command => command.name() === commandName);
        if(next === undefined) {
            console.error(`argon-js-backend: unknown command: ${commandPath.join(" ")}`);
            process.exitCode = 1;
            return;
        }

        current = next;
    }

    console.log(current.description());
}

program
    .command("platform-metadata")
    .description("Load platform-specific metadata")
    .command("js")
    .description("Load platform metadata for JavaScript")
    .option("--package-name <name>", "NPM package name for the generated tube")
    .option("--extern-file <file>", "JavaScript extern file", collect, [])
    .requiredOption("-o, --output-file <file>", "Output platform metadata file")
    .action(platformMetadataJs);

program
    .command("help-description")
    .description("Print a command help description")
    .argument("<command...>", "Command path")
    .action(printHelpDescription);

try {
    await program.parseAsync(process.argv);
}
catch(err) {
    const message = err instanceof Error ? err.message : String(err);
    console.error(`argon-js-backend: ${message}`);
    process.exitCode = 1;
}
