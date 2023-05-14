import type ts from "typescript";
import { ScalaGenerator } from "./scala-common.js";


class ScalaJSGenerator extends ScalaGenerator {
    protected override async writePromiseType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.scalajs.js.Promise");
    }
    protected override async writeMapType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.scalajs.js.Map");
    }
    protected override async writeUint8ArrayType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.scalajs.js.typedarray.Uint8Array");
    }
    protected override async writeBigIntType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.scalajs.js.BigInt");
    }
    protected override async writeArrayType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.scalajs.js.Array");
    }
    protected override async writeFunctionType(out: WritableStreamDefaultWriter<string>, t: ts.FunctionTypeNode, localTypes: readonly string[]): Promise<void> {
        await out.write(`_root_.scala.scalajs.js.Function${t.parameters.length}[`);
        for(const param of t.parameters) {
            if(param.type === undefined) {
                throw new Error("Function type has missing parameter type");
            }
            
            await this.writeType(out, param.type, localTypes);
            await out.write(", ");
        }
        await this.writeType(out, t.type, localTypes);
        await out.write("]");
    }

    protected readonly baseType: string = "_root_.scala.scalajs.js.Object";
    protected readonly contextBounds: readonly string[] = [];

    
}



await new ScalaJSGenerator().generate();
