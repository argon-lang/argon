import type {ResourceFactory, ResourceRecorder, FileSystemResource} from "./resource.js";
import type {Toml} from "./proto/util.js";

export type OptionDecodeResult<A> = OptionDecodeSuccess<A> | OptionDecodeFailure;

export interface OptionDecodeSuccess<A> {
    readonly ok: true;
    readonly value: A;
}

export interface OptionDecodeFailure {
    readonly ok: false;
    readonly errorMessage: string;
}

export interface OptionDecoder<Options> {
    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<Options>;
}

export interface OptionCodec<Options> extends OptionDecoder<Options> {
    encode(recorder: ResourceRecorder, value: Options): Promise<Toml>;
}

export interface OutputInfo<Output> {
    getValue(output: Output): FileSystemResource;
}

export interface OutputHandler<Output> {
    get options(): Map<string, OutputInfo<Output>>;
}


