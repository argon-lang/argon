import type {Dict} from "@argon-lang/noble-idl-core";
import type {
    CompilerDriverOptionValue,
    CompilerDriverOptionValueAtom, CompilerDriverOutput,
    DriverCommand
} from "@argon-lang/node-driver-api/command.js";
import type {
    BinaryResource,
    BinaryResourceSink,
    DirectoryResource,
    DirectoryResourceSink,
} from "@argon-lang/js-backend-api";
import {FSBinaryResource, FSBinaryResourceSink, FSDirectoryResource, FSDirectoryResourceSink} from "./resource.js";


export function convertCommand(options: DriverCommand<string, string, string, string>): DriverCommand<BinaryResource<Error>, DirectoryResource<Error>, BinaryResourceSink<Error>, DirectoryResourceSink<Error>> {
    switch (options.$type) {
        case "help-command":
        case "version-command":
        case "list-backends-command":
        case "rpc":
            return options;

        case "compile-command":
            return {
                $type: "compile-command",
                tubeName: options.tubeName,
                inputDir: new FSDirectoryResource(options.inputDir),
                outputFile: new FSBinaryResourceSink(options.outputFile),
                referencedTubes: options.referencedTubes.map(p => new FSBinaryResource(p)),
                supportedPlatforms: options.supportedPlatforms,
                platformOptions: mapDictValue(options.platformOptions, d => mapDictValue(d, convertOption)),
            };

        case "gen-ir-command":
            return {
                $type: "gen-ir-command",
                inputFile: new FSBinaryResource(options.inputFile),
                outputFile: new FSBinaryResourceSink(options.outputFile),
                referencedTubes: options.referencedTubes.map(p => new FSBinaryResource(p)),
                platform: options.platform,
            };

        case "codegen-command":
            return {
                $type: "codegen-command",
                backend: options.backend,
                inputFile: new FSBinaryResource(options.inputFile),
                referencedTubes: options.referencedTubes.map(p => new FSBinaryResource(p)),
                platformOptions: mapDictValue(options.platformOptions, convertOption),
                platformOutputOptions: mapDictValue(options.platformOutputOptions, convertOutput)
            };
    }
}

function convertOption(option: CompilerDriverOptionValue<string, string>): CompilerDriverOptionValue<BinaryResource<Error>, DirectoryResource<Error>> {
    switch(option.$type) {
        case "single":
            return {
                $type: "single",
                value: convertOptionAtom(option.value),
            }

        case "many":
            return {
                $type: "many",
                first: convertOptionAtom(option.first),
                values: option.values.map(convertOptionAtom),
            };
    }
}

function convertOptionAtom(option: CompilerDriverOptionValueAtom<string, string>): CompilerDriverOptionValueAtom<BinaryResource<Error>, DirectoryResource<Error>> {
    switch(option.$type) {
        case "string":
        case "bool":
            return option;

        case "file":
            return {
                $type: "file",
                f: new FSBinaryResource(option.f),
            };

        case "directory":
            return {
                $type: "directory",
                dir: new FSDirectoryResource(option.dir),
            };
    }
}

function convertOutput(output: CompilerDriverOutput<string, string>): CompilerDriverOutput<BinaryResourceSink<Error>, DirectoryResourceSink<Error>> {
    switch(output.$type) {
        case "file":
            return {
                $type: "file",
                f: new FSBinaryResourceSink(output.f),
            };

        case "directory":
            return {
                $type: "directory",
                dir: new FSDirectoryResourceSink(output.dir),
            };
    }
}

function mapDictValue<A, B>(dict: Dict<A>, f: (a: A) => B): Dict<B> {
    const result = new Map<string, B>();
    for(const [key, value] of dict) {
        result.set(key, f(value));
    }
    return result;
}
