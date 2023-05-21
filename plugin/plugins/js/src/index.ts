import type { LoaderFactory, Plugin, PluginConsumer, PluginFactory, PluginOperations } from "@argon-lang/plugin-api";
import type { OptionCodec, OutputHandler } from "@argon-lang/plugin-api/options";
import type { EmitTubeInfo } from "@argon-lang/plugin-api/tube";
import type { FunctionDeclaration } from "estree";
import { TubeEmitter } from "./emit/tube_emitter.js";
import type { ExternImpl } from "./extern.js";
import { JSOptions, JSOutput } from "./options.js";



class JSPlugin implements Plugin<JSOptions, JSOutput, ExternImpl, ExternImpl, ExternImpl> {
    get optionCodec(): OptionCodec<JSOptions> {
        return JSOptions.codec;
    }
    get outputHandler(): OutputHandler<JSOutput> {
        return JSOutput.handler;
    }

    async emitTube(options: JSOptions, emitInfo: EmitTubeInfo<FunctionDeclaration, FunctionDeclaration, FunctionDeclaration>): Promise<JSOutput> {
        const metadata = await emitInfo.tube.metadata();
        const emitter = new TubeEmitter(options, emitInfo, metadata);
        return emitter.emitTube();
    }
    
    async loadExternMethod(options: JSOptions, id: string): Promise<FunctionDeclaration | null> {
        if(options.extern == null) {
            return null;
        }

        for(const externRes of options.extern) {
            const module = await externRes.asModule();
            for(const stmt of module.body) {
                if(stmt.type !== "ExportNamedDeclaration" || stmt.declaration?.type !== "FunctionDeclaration") {
                    continue;
                }

                if(stmt.declaration.id?.name == id) {
                    return stmt.declaration;
                }
            }
        }

        return null;
    }
    
    loadExternFunction(options: JSOptions, id: string): Promise<FunctionDeclaration | null> {
        return this.loadExternMethod(options, id);
    }

    loadExternClassConstructor(options: JSOptions, id: string): Promise<FunctionDeclaration | null> {
        return this.loadExternMethod(options, id);
    }

    get tubeLoaders(): Map<string, LoaderFactory> {
        return new Map();
    }

}


const pluginFactory: PluginFactory = {
    create: function <A>(operations: PluginOperations, consumer: PluginConsumer<A>): A {
        return consumer.consume(new JSPlugin());
    }
};

export default pluginFactory;
