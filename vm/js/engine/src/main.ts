import { loadProgram } from "@argon-lang/vm-format/lib/program.js";
import * as proto from "@argon-lang/vm-format/proto/argonvm.js";
import * as fs from "node:fs/promises";
import { StandardLibrary } from "./standardlib.js";
import { VM } from "./VM.js";

const infile = process.argv[2];
if(infile === undefined) {
    throw new Error("Missing input file");
}

const programProto = proto.Program.fromBinary(await fs.readFile(infile));

const standardLib = new StandardLibrary();

const [program, entrypoint] = loadProgram(programProto, standardLib.nativeFunctions);

const vm = new VM(program);
await vm.execute(entrypoint);
