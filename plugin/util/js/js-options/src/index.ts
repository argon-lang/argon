import type { BinaryResource, DirectoryResource, ResourceFactory, ResourceRecorder, DirectoryEntry, FileSystemResource } from "@argon-lang/plugin-api/resource";
import type { OptionCodec, OptionDecoder, OptionDecodeResult, OutputInfo, OutputHandler } from "@argon-lang/plugin-api/options";
import type { Toml, TomlKeyValue } from "@argon-lang/plugin-api/tube";


export interface OptionFieldDecoder<Options> extends OptionDecoder<Options> {
    defaultValue?: undefined | Options;
}

export interface OptionFieldCodec<Options> extends OptionDecoder<Options> {
    encode(recorder: ResourceRecorder, value: Options): Promise<Toml | null>;
}


export interface ResourceDecoder<TRes extends BinaryResource> {
    decode(res: BinaryResource): TRes;
}



type ObjectFieldDecoders<T> = {
    [K in keyof T]: OptionFieldDecoder<T[K]>;
};

type ObjectFieldCodecs<T> = {
    [K in keyof T]: OptionFieldCodec<T[K]>;
};

class ObjectDecoder<T> implements OptionDecoder<T> {
    constructor(fieldDecoders: ObjectFieldDecoders<T>) {
        this.#fieldDecoders = fieldDecoders;
    }

    #fieldDecoders: ObjectFieldDecoders<T>;

    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<T> {
        if(value.$case == "TomlTable") {
            const elements = value.table;
            const o: any = {};
            for(const key of Object.keys(this.#fieldDecoders) as (keyof T & string)[]) {
                const elemValue = elements.find(kvp => kvp.key == key)?.value;
                if(elemValue === undefined) {
                    const defaultValue = this.#fieldDecoders[key].defaultValue;
                    if(defaultValue === undefined) {
                        return { errorMessage: "Missing entry in table: " + key };
                    }
                    else {
                        o[key] = defaultValue;
                    }
                }
                else {
                    const res = this.#fieldDecoders[key].decode(resourceFactory, elemValue);
                    if("value" in res) {
                        o[key] = res.value;   
                    }
                    else {
                        return res;
                    }
                }
            }
            return { value: o };
        }
        else {
            return { errorMessage: "Expected table value" };
        }
    }
}

class ObjectCodec<T> extends ObjectDecoder<T> implements OptionCodec<T> {
    constructor(fieldCodecs: ObjectFieldCodecs<T>) {
        super(fieldCodecs);
        this.#fieldCodecs = fieldCodecs;
    }

    #fieldCodecs: ObjectFieldCodecs<T>;

    async encode(resourceRecorder: ResourceRecorder, value: T): Promise<Toml> {
        const elements: TomlKeyValue[] = [];
        for(const key of Object.keys(this.#fieldCodecs) as (keyof T & string)[]) {
            const elemValue = await this.#fieldCodecs[key].encode(resourceRecorder, value[key]);
            if(elemValue !== null) {
                elements.push({
                    key,
                    value: elemValue,
                });
            }
        }
        return {
            $case: "TomlTable",
            table: elements,
        };
    }
}

export function objectDecoder<T>(o: ObjectFieldDecoders<T>): ObjectDecoder<T> {
    return new ObjectDecoder<T>(o);
}

export function objectCodec<T>(o: ObjectFieldCodecs<T>): OptionCodec<T> {
    return new ObjectCodec<T>(o);
}


class ArrayDecoder<T> implements OptionDecoder<readonly T[]> {
    constructor(elementDecoder: OptionDecoder<T>) {
        this.#elementDecoder = elementDecoder;
    }

    #elementDecoder: OptionDecoder<T>;


    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<readonly T[]> {
        if(value.$case == "TomlArray") {
            const elements: T[] = [];
            for(const element of value.values) {
                const res = this.#elementDecoder.decode(resourceFactory, element);
                if("value" in res) {
                    elements.push(res.value);
                }
                else {
                    return res;
                }
            }
            return { value: elements };
        }
        else {
            return { errorMessage: "Expected array value" };
        }
    }
}

class ArrayCodec<T> extends ArrayDecoder<T> implements OptionCodec<readonly T[]> {
    constructor(elementCodec: OptionCodec<T>) {
        super(elementCodec);
        this.#elementCodec = elementCodec;
    }

    #elementCodec: OptionCodec<T>;


    async encode(recorder: ResourceRecorder, value: readonly T[]): Promise<Toml> {
        const elements: Toml[] = [];
        for(const t of value) {
            elements.push(await this.#elementCodec.encode(recorder, t));
        }
        return {
            $case: "TomlArray",
            values: elements,
        };
    }
}


export function arrayDecoder<T>(elementDecoder: OptionDecoder<T>): OptionDecoder<readonly T[]> {
    return new ArrayDecoder<T>(elementDecoder);
}

export function arrayCodec<T>(elementCodec: OptionCodec<T>): OptionCodec<readonly T[]> {
    return new ArrayCodec<T>(elementCodec);
}


class NullableDecoder<T> implements OptionFieldDecoder<T | null> {
    constructor(elementDecoder: OptionDecoder<T>) {
        this.#elementDecoder = elementDecoder;
    }

    #elementDecoder: OptionDecoder<T>;
    

    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<T | null> {
        return this.#elementDecoder.decode(resourceFactory, value);
    }

    get defaultValue() {
        return null;
    }
}

class NullableCodec<T> extends NullableDecoder<T> implements OptionFieldCodec<T | null> {
    constructor(elementCodec: OptionCodec<T>) {
        super(elementCodec);
        this.#elementCodec = elementCodec;
    }

    #elementCodec: OptionCodec<T>;


    async encode(recorder: ResourceRecorder, value: T | null): Promise<Toml | null> {
        if(value === null) {
            return null;
        }
        
        return this.#elementCodec.encode(recorder, value);
    }
}

export function nullableDecoder<T>(elementDecoder: OptionDecoder<T>): OptionFieldDecoder<T | null> {
    return new NullableDecoder<T>(elementDecoder);
}

export function nullableCodec<T>(elementCodec: OptionCodec<T>): OptionFieldCodec<T | null> {
    return new NullableCodec<T>(elementCodec);
}


class MapDecoder<T> implements OptionDecoder<ReadonlyMap<string, T>> {
    constructor(elementDecoder: OptionDecoder<T>) {
        this.#elementDecoder = elementDecoder;
    }

    #elementDecoder: OptionDecoder<T>;


    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<ReadonlyMap<string, T>> {
        if(value.$case == "TomlTable") {
            const elements = new Map<string, T>();
            for(const element of value.table) {
                const res = this.#elementDecoder.decode(resourceFactory, element.value!);
                if("value" in res) {
                    elements.set(element.key, res.value);
                }
                else {
                    return res;
                }
            }
            return { value: elements };
        }
        else {
            return { errorMessage: "Expected table value" };
        }
    }
}

class MapCodec<T> extends MapDecoder<T> implements OptionCodec<ReadonlyMap<string, T>> {
    constructor(elementCodec: OptionCodec<T>) {
        super(elementCodec);
        this.#elementCodec = elementCodec;
    }

    #elementCodec: OptionCodec<T>;


    async encode(recorder: ResourceRecorder, value: ReadonlyMap<string, T>): Promise<Toml> {
        const elements: TomlKeyValue[] = [];
        for(const [key, v] of value) {
            elements.push({
                key,
                value: await this.#elementCodec.encode(recorder, v),
            });
        }
        return {
            $case: "TomlTable",
            table: elements,
        };
    }
}


export function mapDecoder<T>(elementDecoder: OptionDecoder<T>): OptionDecoder<ReadonlyMap<string, T>> {
    return new MapDecoder<T>(elementDecoder);
}

export function mapCodec<T>(elementCodec: OptionCodec<T>): OptionCodec<ReadonlyMap<string, T>> {
    return new MapCodec<T>(elementCodec);
}




export const stringCodec: OptionCodec<string> = {
    async encode(recorder: ResourceRecorder, value: string): Promise<Toml> {
        return {
            $case: "StringValue",
            s: value,
        };
    },
    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<string> {
        if(value.$case === "StringValue") {
            return { value: value.s };
        }
        else {
            return { errorMessage: "Expected string value" };
        }
    },
};


export const bigintCodec: OptionCodec<bigint> = {
    async encode(recorder: ResourceRecorder, value: bigint): Promise<Toml> {
        return {
            $case: "IntValue",
            i: value,
        };
    },
    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<bigint> {
        if(value.$case === "IntValue") {
            return { value: value.i };
        }
        else {
            return { errorMessage: "Expected int value" };
        }
    },
};

export const numberCodec: OptionCodec<number> = {
    async encode(recorder: ResourceRecorder, value: number): Promise<Toml> {
        return {
            $case: "FloatValue",
            f: value,
        };
    },
    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<number> {
        if(value.$case === "FloatValue") {
            return { value: value.f };
        }
        else {
            return { errorMessage: "Expected float value" };
        }
    },
};

export const booleanCodec: OptionCodec<boolean> = {
    async encode(recorder: ResourceRecorder, value: boolean): Promise<Toml> {
        return {
            $case: "BoolValue",
            b: value,
        };
    },
    decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<boolean> {
        if(value.$case === "BoolValue") {
            return { value: value.b };
        }
        else {
            return { errorMessage: "Expected bool value" };
        }
    },
};



export const binaryResourceDecoder: ResourceDecoder<BinaryResource> = {
    decode(res: BinaryResource): BinaryResource {
        return res;
    },
};


function decodeDirectoryEntry<TRes extends BinaryResource>(entry: DirectoryEntry<BinaryResource>, decoder: ResourceDecoder<TRes>): DirectoryEntry<TRes> {
    if(entry.entryType == "file") {
        return {
            entryType: "file",
            name: entry.name,
            resource: decoder.decode(entry.resource),
        };
    }
    else {
        return {
            entryType: "subdirectory",
            name: entry.name,
            resource: decodeDirectoryResource(entry.resource, decoder),
        }
    }
}

function decodeDirectoryResource<TRes extends BinaryResource>(res: DirectoryResource<BinaryResource>, decoder: ResourceDecoder<TRes>): DirectoryResource<TRes> {
    return {
        resourceType: "directory",
        contents: async function*() {
            for await(const entry of res.contents()) {
                yield decodeDirectoryEntry(entry, decoder);
            }
        },
        fileName: res.fileName,
    };
}

export function directoryResourceCodec<TRes extends BinaryResource>(decoder: ResourceDecoder<TRes>): OptionCodec<DirectoryResource<TRes>> {
    return {
        decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<DirectoryResource<TRes>> {
            if(value.$case === "IntValue") {
                return {
                    value: decodeDirectoryResource(resourceFactory.directoryResource(value.i), decoder),
                };
            }
            else {
                return { errorMessage: "Expected string value" };
            }
        },

        async encode(recorder: ResourceRecorder, value: DirectoryResource<TRes>): Promise<Toml> {
            return {
                $case: "IntValue",
                i: await recorder.recordDirectoryResource(value),
            };
        },
    };
}

export function binaryResourceCodec<TRes extends BinaryResource>(decoder: ResourceDecoder<TRes>): OptionCodec<TRes> {
    return {
        decode(resourceFactory: ResourceFactory, value: Toml): OptionDecodeResult<TRes> {
            if(value.$case === "IntValue") {
                return {
                    value: decoder.decode(resourceFactory.binaryResource(value.i)),
                };
            }
            else {
                return { errorMessage: "Expected string value" };
            }
        },

        async encode(recorder: ResourceRecorder, value: TRes): Promise<Toml> {
            return {
                $case: "IntValue",
                i: await recorder.recordBinaryResource(value),
            };
        },
    };
}




async function* encodeUTF8(text: AsyncIterable<string>): AsyncIterable<Uint8Array> {
    const encoder = new TextEncoder();
    for await(const s of text) {
        yield encoder.encode(s);
    }
}

async function* decodeUTF8(bin: AsyncIterable<Uint8Array>): AsyncIterable<string> {
    const decoder = new TextDecoder();
    for await(const b of bin) {
        yield decoder.decode(b, { stream: true });
    }

    yield decoder.decode();
}


export abstract class TextResource implements BinaryResource {
    readonly resourceType: "binary" = "binary";
    abstract get fileName(): string | null;

    abstract asText(): AsyncIterable<string>;

    asAsyncIterable(): AsyncIterable<Uint8Array> {
        return encodeUTF8(this.asText());
    }

    async asString(): Promise<string> {
        let result = "";
        for await(const s of this.asText()) {
            result += s;
        }
        return result;
    }
}

class TextResourceDecoded extends TextResource {
    constructor(res: BinaryResource) {
        super();
        this.#res = res;
    }

    #res: BinaryResource;

    override get fileName(): string | null {
        return this.#res.fileName;
    }

    override asText(): AsyncIterable<string> {
        return decodeUTF8(this.#res.asAsyncIterable());
    }
}

export const textResourceDecoder: ResourceDecoder<TextResource> = {
    decode(res: BinaryResource): TextResource {
        return new TextResourceDecoded(res);
    }
};


interface OutputInfoField<Output, Name extends keyof Output> extends OutputInfo<Output> {
    getValue(output: Output): Output[Name] & FileSystemResource;
}

type OutputFieldHandlers<T> = {
    [K in keyof T]: OutputInfoField<T, K>;
};

export function objectOutputHandler<T>(handlers: OutputFieldHandlers<T>): OutputHandler<T> {
    return {
        get options() {
            const map = new Map<string, OutputInfo<T>>();
            for(const key of Object.keys(handlers) as (keyof T & string)[]) {
                map.set(key, handlers[key]);
            }
            return map;
        }
    };
}

export function outputInfoField<Name extends string>(name: Name): OutputInfoField<{ [K in Name]: FileSystemResource }, Name> {
    return {
        getValue(output) {
            return output[name];
        },
    }
}



