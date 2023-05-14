import type { BinaryResource } from "@argon-lang/plugin-api/resource";
import { ResourceDecoder, TextResource, textResourceDecoder } from "@argon-lang/plugin-options-util";
import type { Program } from "estree";
import * as Astring from "astring";
import * as Acorn from "acorn";

async function* encodeProgram(program: Promise<Program>): AsyncIterable<string> {
    yield Astring.generate(await program);
}

export abstract class JSProgramResource extends TextResource {
    abstract asModule(): Promise<Program>;

    override asText(): AsyncIterable<string> {
        return encodeProgram(this.asModule());
    }
}

class JSProgramResourceParse extends JSProgramResource {
    constructor(res: TextResource) {
        super();
        this.#res = res;
    }

    #res: TextResource;

    override get fileName(): string | null {
        return this.#res.fileName;
    }

    override async asModule(): Promise<Program> {
        return Acorn.parse(await this.#res.asString(), {
            ecmaVersion: 2022,
            sourceType: "module",
            locations: true,
            sourceFile: this.fileName == null ? undefined : this.fileName,
        }) as unknown as Program;
    }
}

export const jsProgramResourceDecoder: ResourceDecoder<JSProgramResource> = {
    decode(res: BinaryResource): JSProgramResource {
        const textRes = textResourceDecoder.decode(res);
        return new JSProgramResourceParse(textRes);
    },
};
