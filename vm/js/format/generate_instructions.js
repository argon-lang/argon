import { readFile, open } from "node:fs/promises";
import { Writable } from "node:stream";


/**
 * 
 * @param {*} instructions 
 * @param {WritableStreamDefaultWriter<string>} stream 
 */
async function generateOutput(instructions, stream) {
    await stream.write("import { readIndex, InstructionContext } from \"./instruction-util.js\";\n");
    await stream.write("export type Instruction = " + Object.keys(instructions).map(opcode => "Instruction." + opcode).join(" | ") + ";\n");
    await stream.write("export namespace Instruction {\n");

    for(const key in instructions) {
        if(!Object.prototype.hasOwnProperty.call(instructions, key)) {
            continue;
        }

        const insn = normalizeInstruction(instructions[key]);
        await stream.write(`\texport interface ${key} {\n`);
        await stream.write(`\t\top: "${key}";\n`);
        for(const param of insn.parameters) {
            await stream.write(`\t\treadonly ${param.name}: ${getType(param.type)};\n`);
        }
        await stream.write("\t}\n");
    }

    await stream.write("\texport function read(ctx: InstructionContext): Instruction {\n");
    await stream.write("\t\tswitch(ctx.chunk.bytecode[ctx.ip++]) {\n");
    for(const key in instructions) {
        if(!Object.prototype.hasOwnProperty.call(instructions, key)) {
            continue;
        }

        const insn = normalizeInstruction(instructions[key]);


        await stream.write(`\t\t\tcase ${insn.opcode}:\n`);
        await stream.write("\t\t\t{\n");
        
        for(const param of insn.parameters) {
            await stream.write(`\t\t\t\tconst ${param.name} = `);
            switch(param.type) {
                case "index":
                    stream.write("readIndex(ctx)");
            }

            await stream.write(";\n");
        }
        await stream.write(`\t\t\t\treturn { op: "${key}", ${insn.parameters.map(param => param.name).join(", ")} };\n`);
        await stream.write("\t\t\t}\n");
    }
    await stream.write("\t\t\tdefault: throw new Error(\"Invalid opcode\");\n");
    await stream.write("\t\t}\n");
    await stream.write("\t}\n");

    await stream.write("}\n");
}

/**
 * @param {string} t 
 */
function getType(t) {
    switch(t) {
        case "index":
            return "bigint";

        default:
            throw new Error("Unsupported prameter type: " + t);
    }
}

function normalizeInstruction(instruction) {
    if(typeof(instruction) == "number") {
        return {
            opcode: instruction,
            parameters: [],
        };
    }
    else {
        return instruction;
    }
}


const instructionsJson = JSON.parse(await readFile("../../bytecode.json", { encoding: "utf-8" }));

const outFile = await open("src/instructions.ts", "w");
try {
    await generateOutput(instructionsJson, Writable.toWeb(outFile.createWriteStream()).getWriter());
}
finally {
    await outFile.close();
}

