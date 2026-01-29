import type {BackendFactory, SimpleBackendFactory} from "@argon-lang/js-backend-api/factory.js";
import type {BackendMetadata} from "@argon-lang/js-backend-api/metadata.js";
import * as path from "node:path";
import * as fs from "node:fs/promises";
import * as url from "node:url";
import type {CompilerDriver} from "@argon-lang/node-driver-api";
import {JSBackendFactory} from "./JSBackendFactory.js";

export async function loadBackendFactories(driver: CompilerDriver): Promise<readonly BackendFactory[]> {
    const backendsDir = path.join(import.meta.dirname, "../../backends");

    const factories: BackendFactory[] = [];
    for(const backendDirName of await fs.readdir(backendsDir)) {
        const backendDir = path.join(backendsDir, backendDirName);
        const metadataPath = path.join(backendDir, "backend.toml");
        const metadata = await loadMetadata(driver, metadataPath);
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

async function loadMetadata(driver: CompilerDriver, path: string): Promise<BackendMetadata | undefined> {
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
                return new JSBackendFactory(metadata, simpleFactory);
            }
        }
    }

    return undefined;
}


