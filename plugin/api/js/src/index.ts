import type { OptionDecoder, OptionCodec, OutputHandler } from "./options.js";
import type { SerializedTube, SerializedTubePlus } from "./tube.js";
import type { TubeName } from "./proto/tube.js";


export interface TubeImporter {
    getTube(tubeName: TubeName): Promise<SerializedTube>;
}

export interface TubeLoader<LibOptions> {
    get libOptionsDecoder(): OptionDecoder<LibOptions>;

    load(tubeImporter: TubeImporter, libOptions: LibOptions): Promise<SerializedTube>;
}

export interface LoaderConsumer<A> {
    consume<LibOptions>(loader: TubeLoader<LibOptions>): A;
}

export interface LoaderFactory {
    withLoader<A>(consumer: LoaderConsumer<A>): A;
}

export interface PluginOperations {

}

export interface Plugin<Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation> {
    get optionCodec(): OptionCodec<Options>;
    get outputHandler(): OutputHandler<Output>;

    emitTube(options: Options, tube: SerializedTubePlus<ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation>): Promise<Output>;

    loadExternMethod(options: Options, id: string): Promise<ExternMethodImplementation | null>;
    loadExternFunction(options: Options, id: string): Promise<ExternFunctionImplementation | null>;
    loadExternClassConstructor(options: Options, id: string): Promise<ExternClassConstructorImplementation | null>;


    get tubeLoaders(): Map<string, LoaderFactory>;
}

export interface PluginConsumer<A> {
    consume<Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation>(
        plugin: Plugin<Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation>
    ): A;
}


export interface PluginFactory {
    create<A>(
        operations: PluginOperations,
        consumer: PluginConsumer<A>,
    ): A;
}

