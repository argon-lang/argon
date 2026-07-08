import * as astring from "astring";
import { loadExterns } from "./externs.js";
import { emitTube, type EmitOptions } from "./emitter.js";
import { readIRFile } from "./ir-reader.js";
import { getModuleOutputFileParts, getModulePathExternalUrl, getModulePathUrl, tubePackageName } from "./util.js";
import type { PackageJson, ReadonlyDeep } from "type-fest";
import {type JSPlatformMetadataOptions, JsPlatformTubeMetadata} from "@argon-lang/js-backend-api";
import type { PlatformMetadataResult } from "@argon-lang/js-backend-api/metadata.js";
import type {JSCodeGenOptions} from "@argon-lang/js-backend-api";

function emitMainJS(): string {
    return "import * as rt from \"@argon-lang/runtime\";\n" +
				"import { main$a$t$e$r$t$e } from \"./lib/index.js\";\n" +
				"rt.resolve(main$a$t$e$r$t$e());\n";
}

function emitPackageJson(options: EmitOptions): ReadonlyDeep<PackageJson> {
    const exports: PackageJson.ExportConditions = {};

    for(const module of options.program.modules) {
        const internalPath = getModulePathUrl(module.path);
        const externalPath = getModulePathExternalUrl(module.path);
        exports[externalPath === "" ? "." : "./" + externalPath] = "./lib/" + internalPath;
    }


    const packageJson: PackageJson = {
        name: tubePackageName(options.program.getTubeInfo(0n)),
        private: true,
        type: "module",
        exports,
    };

    if(options.executable) {
        packageJson.bin = Object.fromEntries([
            [options.executable, "./main.js"],
        ]);
    }

    return packageJson;
}


export async function loadMetadata(options: JSPlatformMetadataOptions): Promise<PlatformMetadataResult> {
    const tubeMetadataObj: JsPlatformTubeMetadata = {
        packageName: options.packageName,
    };

    const tubeMetadata = JsPlatformTubeMetadata.codec.encode(tubeMetadataObj);


    const externs = await loadExterns(options.externFiles);
    return {
        platform: "js",
        tubeMetadata,
        externs,
    };
}

export async function codegen(options: JSCodeGenOptions): Promise<void> {
    const program = await readIRFile(options.tube);

    const emitOptions: EmitOptions = {
        program,
        executable: options.executable,
    };

    for(const outputModule of emitTube(emitOptions)) {
        const outputFileParts = getModuleOutputFileParts(outputModule.modulePath);

        const program = outputModule.emitJsProgram();
        const programStr = astring.generate(program);

        const file = options.outputDirectory.getFile("lib", ...outputFileParts[0], outputFileParts[1]);

        const stream = await file.open();
        try {
            await stream.write(new TextEncoder().encode(programStr));
        }
        finally {
            await stream.close();
        }
    }

    if(options.executable) {
        const mainJS = emitMainJS();
        const mainJSFile = options.outputDirectory.getFile("main.js");
        const stream = await mainJSFile.open();
        try {
            await stream.write(new TextEncoder().encode(mainJS));
        }
        finally {
            await stream.close();
        }
    }

    {
        const packageJson = emitPackageJson(emitOptions);
        const packageJsonStr = JSON.stringify(packageJson, null, 2);
        const packageJsonFile = options.outputDirectory.getFile("package.json");
        const stream = await packageJsonFile.open();
        try {
            await stream.write(new TextEncoder().encode(packageJsonStr));
        }
        finally {
            await stream.close();
        }
    }
}

