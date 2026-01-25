import {compilerDriver as driver} from "./argon.js";
import type {SimpleBackendFactory, BackendFactory} from "@argon-lang/js-backend-api/factory.js";
import type {BackendMetadata} from "@argon-lang/js-backend-api/metadata.js";
import type {CompilerDriverOptions} from "@argon-lang/node-driver-api";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import * as url from "node:url";
import {JSBackendFactory} from "./JSBackendFactory.js";

const backendFactories = await loadBackendFactories();
try {
    const backendMetadata = backendFactories.map(f => f.metadata);

    console.log(backendMetadata);

    const args = process.argv.slice(2);
    const command = driver.parseCommandLineArguments(backendMetadata, args);

    const options: CompilerDriverOptions = {
        backendFactories,
        command,
    }

    process.exitCode = await driver.runCommand(options);
}
finally {
    for(const factory of backendFactories) {
        await factory.close();
    }
}


async function loadBackendFactories(): Promise<readonly BackendFactory[]> {
    const backendsDir = path.join(import.meta.dirname, "../../backends");

    const factories: BackendFactory[] = [];
    for(const backendDirName of await fs.readdir(backendsDir)) {
        const backendDir = path.join(backendsDir, backendDirName);
        const metadataPath = path.join(backendDir, "backend.toml");
        const metadata = await loadMetadata(metadataPath);
        if(metadata === undefined) {
            continue;
        }

        const factory = await loadBackendFactory(backendDir, metadata);
        if(factory !== undefined) {
            factories.push(factory);
        }
    }

    return factories;
}

async function loadMetadata(path: string): Promise<BackendMetadata | undefined> {
    let content: string;
    try {
        content = await fs.readFile(path, "utf-8");
    }
    catch(e) {
        if(e instanceof Error && ("code" in e) && e.code === "ENOENT") {
            return undefined;
        }

        throw new Error(`Failed to read metadata file: ${path}`, { cause: e });
    }

    try {
        return driver.parseMetadata(content);
    }
    catch(error) {
        throw new Error(`Failed to parse metadata file: ${path}`, { cause: error });
    }
}

async function loadBackendFactory(backendDir: string, metadata: BackendMetadata): Promise<BackendFactory | undefined> {
    for(const loader of metadata.loaders) {
        switch(loader.$type) {
            case "js":
            {
                const importPath = path.join(backendDir, loader.jsOptions.importPath);
                const importUrl = url.pathToFileURL(importPath);
                const factoryModule = await import(importUrl.href);
                const simpleFactory: SimpleBackendFactory = factoryModule[loader.jsOptions.exportName];
                console.log(simpleFactory);
                return new JSBackendFactory(metadata, simpleFactory);
            }
        }
    }

    return undefined;
}
