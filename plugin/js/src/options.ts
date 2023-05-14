import type { OptionCodec, OutputHandler } from "@argon-lang/plugin-api/options";
import type { DirectoryResource } from "@argon-lang/plugin-api/resource";
import { objectOutputHandler, outputInfoField } from "@argon-lang/plugin-options-util";
import { arrayCodec, binaryResourceCodec, mapCodec, nullableCodec, objectCodec, stringCodec } from "@argon-lang/plugin-options-util";
import { JSProgramResource, jsProgramResourceDecoder } from "./resource.js";



export interface TubeOptions {
    import_path: string;
}

export namespace TubeOptions {
    export const codec: OptionCodec<TubeOptions> = objectCodec({
        import_path: stringCodec,
    });
}

export interface ModuleOptions {
    readonly inject_before: JSProgramResource | null;
    readonly inject_after: JSProgramResource | null;
}

export namespace ModuleOptions {
    export const codec: OptionCodec<ModuleOptions> = objectCodec({
        inject_before: nullableCodec(binaryResourceCodec(jsProgramResourceDecoder)),
        inject_after: nullableCodec(binaryResourceCodec(jsProgramResourceDecoder)),
    });
}

export interface JSOptions {
    readonly extern: readonly JSProgramResource[] | null;
    readonly modules: ReadonlyMap<string, ModuleOptions> | null;
    readonly tubes: ReadonlyMap<string, TubeOptions> | null;
}

export namespace JSOptions {
    export const codec: OptionCodec<JSOptions> = objectCodec({
        extern: nullableCodec(arrayCodec(binaryResourceCodec(jsProgramResourceDecoder))),
        modules: nullableCodec(mapCodec(ModuleOptions.codec)),
        tubes: nullableCodec(mapCodec(TubeOptions.codec)),
    });
}

export interface JSOutput {
    readonly package: DirectoryResource<JSProgramResource>,
}

export namespace JSOutput {
    export const handler: OutputHandler<JSOutput> = objectOutputHandler({
        package: outputInfoField("package"),
    });
}

