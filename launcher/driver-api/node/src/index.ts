import type {BackendFactory} from "@argon-lang/js-backend-api/factory.js";
import type {DriverCommand} from "./command.js";
import type {BackendMetadata} from "@argon-lang/js-backend-api/metadata.js";

export interface CompilerDriver {
    parseMetadata(metadata: string): BackendMetadata;
    parseCommandLineArguments(backends: readonly BackendMetadata[], args: readonly string[]): DriverCommand<string>;
    runCommand(options: CompilerDriverOptions): Promise<number>;
}

export interface CompilerDriverOptions {
    readonly backendFactories: readonly BackendFactory[];
    readonly command: DriverCommand<string>;
}
