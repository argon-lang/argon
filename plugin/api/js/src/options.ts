import type {ResourceFactory, ResourceRecorder, FileSystemResource} from "./resource.js";
import type {Toml} from "./proto/util.js";

export type OptionDecodeResult<A> = { readonly value: A } | { readonly errorMessage: string };

export interface OptionDecoder<Options> {
    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<Options>;
    get defaultValue(): Options | null;
}

export interface OptionCodec<Options> extends OptionDecoder<Options> {
    encode(recorder: ResourceRecorder, value: Options): Promise<Toml>;
}

export interface OutputInfo<Output> {
    getValue(output: Output): FileSystemResource;
}

export interface OutputHandler<Output> {
    get options(): Map<readonly string[], OutputInfo<Output>>;
}


