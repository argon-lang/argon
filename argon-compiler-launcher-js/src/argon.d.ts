
export interface CompilerDriverInterface {
    runCommand(options: CompilerDriverJSOptions): Promise<number>;
}

export interface CompilerDriverJSOptions {
    readonly pluginDirectories: readonly string[];
    readonly arguments: readonly string[];
}

export const compilerDriver: CompilerDriverInterface;
