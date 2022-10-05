import { NativeFunctions } from "@argon-lang/vm-format/lib/vmfunction.js";
import { VMType } from "@argon-lang/vm-format/lib/vmtype.js";

export class StandardLibrary {

    constructor() {}

    print(value: string): void {
        console.log(value);
    }

    int32_to_string(value: number): string {
        return value.toString();
    }

    uint32_to_string(value: number): string {
        return (value >>> 0).toString();
    }

    float64_to_string(value: number): string {
        return value.toString();
    }

    nativeFunctions: NativeFunctions = (() => {
        const stdlib = this;
        return {
            print: {
                native: true,
                parameterTypes: [ VMType.ObjectReference ],
                returnType: VMType.tuple(),
                async invoke(...args) {
                    stdlib.print(args[0] as string);
                    return { type: "result", value: [] };
                }
            },

            int32_to_string: {
                native: true,
                parameterTypes: [ VMType.Int32 ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    const result = stdlib.int32_to_string(args[0] as number);
                    return { type: "result", value: result };
                }
            },

            uint32_to_string: {
                native: true,
                parameterTypes: [ VMType.Int32 ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    const result = stdlib.uint32_to_string(args[0] as number);
                    return { type: "result", value: result };
                }
            },

            float64_to_string: {
                native: true,
                parameterTypes: [ VMType.Int32 ],
                returnType: VMType.ObjectReference,
                async invoke(...args) {
                    const result = stdlib.float64_to_string(args[0] as number);
                    return { type: "result", value: result };
                }
            },
        };
    })();
}
