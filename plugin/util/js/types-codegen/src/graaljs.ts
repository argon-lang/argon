import ts from "typescript";
import { ScalaGenerator } from "./scala-common.js";


class GraalJSGenerator extends ScalaGenerator {
    protected override async writePromiseType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.dev.argon.plugin.loader.js.JSPromise");
    }
    protected override async writeMapType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.collection.immutable.Map");
    }
    protected override async writeUint8ArrayType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.zio.Chunk[_root_.scala.Byte]");
    }
    protected override async writeBigIntType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.math.BigInt");
    }
    protected override async writeArrayType(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write("_root_.scala.collection.immutable.Seq");
    }
    protected override async writeFunctionType(out: WritableStreamDefaultWriter<string>, t: ts.FunctionTypeNode, localTypes: readonly string[]): Promise<void> {
        await out.write("(");
        for(const [i, param] of t.parameters.entries()) {
            if(param.type === undefined) {
                throw new Error("Function type has missing parameter type");
            }
            
            await this.writeType(out, param.type, localTypes);
            if(i < t.parameters.length - 1) {
                await out.write(", ");
            }
        }
        await out.write(") => ")
        await this.writeType(out, t.type, localTypes);
    }

    private async writeTypeParametersAsArguments(out: WritableStreamDefaultWriter<string>, typeParams: readonly ts.TypeParameterDeclaration[] | undefined): Promise<void> {
        if(typeParams !== undefined && typeParams.length > 0) {
            await out.write("[");
            for(const [i, typeParam] of typeParams.entries()) {
                await this.writeId(out, typeParam.name);    
                if(i < typeParams.length - 1) {
                    await out.write(", ");
                }
            }
            await out.write("]");
        }
    }

    protected readonly baseType: string = "_root_.org.graalvm.polyglot.proxy.ProxyObject";
    protected readonly contextBounds: readonly string[] = ["_root_.dev.argon.plugin.loader.js.ValueEncoder", "_root_.dev.argon.plugin.loader.js.ValueDecoder"];
    

    protected override async generateAdditionalInterfaceMethods(out: WritableStreamDefaultWriter<string>, node: ts.InterfaceDeclaration, localTypes: readonly string[]): Promise<void> {
        await this.writeIndent(out, 1);
        await out.write("override def getMember(key: String): Any =\n");

        for(const [i, member] of node.members.entries()) {
            await this.writeIndent(out, 2);
            if(i > 0) {
                await out.write("else ");
            }
            await out.write("if key == \"");

            if(ts.isMethodSignature(member) || ts.isPropertySignature(member) || ts.isGetAccessor(member)) {
                await this.writeProp(out, member.name);
            }
            else {
                console.log(member);
                throw new Error("Unexpected member");
            }

            await out.write("\" then\n");


            if(ts.isMethodSignature(member)) {
                const localTypes2 = [ ...localTypes, ...member.typeParameters?.map(p => this.getIdName(p.name)) ?? [] ];

                await this.writeIndent(out, 3);
                await out.write("import _root_.dev.argon.plugin.loader.js.given\n");
                await this.writeIndent(out, 3);
                await out.write(`import _root_.${this.getPackageForFile("index.ts")}.given\n`);
                await this.writeIndent(out, 3);
                await out.write(`import _root_.${this.getPackageForFile("tube.ts")}.given\n`);
                if(member.typeParameters !== undefined) {
                    for(const typeParam of member.typeParameters) {
                        await this.writeIndent(out, 3);
                        await out.write("type ");
                        await this.writeId(out, typeParam.name);
                        await out.write(" = _root_.org.graalvm.polyglot.Value\n");
                    }
                }
                await this.writeIndent(out, 3);
                await out.write(`_root_.dev.argon.plugin.loader.js.function${member.parameters.length}ValueEncoder[`);
                for(const [i, param] of member.parameters.entries()) {
                    if(param.type === undefined) {
                        throw new Error("Missing type on parameter");
                    }

                    await this.writeType(out, param.type, localTypes2);
                    await out.write(", ");
                }
                if(member.type === undefined) {
                    throw new Error("Missing return type");
                }
                await this.writeType(out, member.type, localTypes2);
                await out.write("].encode(")
                await this.writeProp(out, member.name);
                await out.write(")\n");
                
            }
            else if(ts.isPropertySignature(member) || ts.isGetAccessor(member)) {
                if(member.type === undefined) {
                    throw new Error("Missing property type");
                }

                await this.writeIndent(out, 3);
                await out.write("import _root_.dev.argon.plugin.loader.js.given\n");
                await this.writeIndent(out, 3);
                await out.write(`import _root_.${this.getPackageForFile("index.ts")}.given\n`);
                await this.writeIndent(out, 3);
                await out.write(`import _root_.${this.getPackageForFile("tube.ts")}.given\n`);
                await this.writeIndent(out, 3);
                await out.write("summon[_root_.dev.argon.plugin.loader.js.ValueEncoder[");
                await this.writeType(out, member.type, localTypes);
                await out.write("]].encode(");
                await this.writeProp(out, member.name);
                await out.write(")\n");
            }
            else {
                console.log(member);
                throw new Error("Unexpected member");
            }
        }

        
        if(node.members.length === 0 && (node.heritageClauses === undefined || node.heritageClauses.length === 0)) {
            await this.writeIndent(out, 2);
            await out.write("throw new RuntimeException(s\"Unexpected key: $key\")\n")
        }
        else {
            await this.writeIndent(out, 2);
            await out.write("else\n");
            await this.writeIndent(out, 3);
            await out.write("throw new RuntimeException(s\"Unexpected key: $key\")\n")
            await this.writeIndent(out, 2);
            await out.write("end if\n");    
        }

        await this.writeIndent(out, 1);
        await out.write("override def getMemberKeys(): Any = (Set[String](");

        for(const [i, member] of node.members.entries()) {
            if(i > 0) {
                await out.write(", ");
            }

            await out.write("\"");
            if(ts.isMethodSignature(member) || ts.isPropertySignature(member) || ts.isGetAccessor(member)) {
                await this.writeProp(out, member.name);
            }
            else {
                console.log(member);
                throw new Error("Unexpected member");
            }
            await out.write("\"");
        }
        await out.write(")");

        await out.write(").toArray\n");




        await this.writeIndent(out, 1);
        await out.write("override def hasMember(key: String): Boolean =\n");

        for(const [i, member] of node.members.entries()) {
            await this.writeIndent(out, 2);
            if(i > 0) {
                await out.write("else ");
            }
            await out.write("if key == \"");

            if(ts.isMethodSignature(member) || ts.isPropertySignature(member) || ts.isGetAccessor(member)) {
                await this.writeProp(out, member.name);
            }
            else {
                console.log(member);
                throw new Error("Unexpected member");
            }

            await out.write("\" then\n");


            await this.writeIndent(out, 3);
            await out.write("true\n")
        }

        
        if(node.members.length === 0 && (node.heritageClauses === undefined || node.heritageClauses.length === 0)) {
            await this.writeIndent(out, 2);
            await out.write("false\n")
        }
        else {
            await this.writeIndent(out, 2);
            await out.write("else\n");
            await this.writeIndent(out, 3);
            await out.write("false\n")
            await this.writeIndent(out, 2);
            await out.write("end if\n");    
        }

        await this.writeIndent(out, 1);
        await out.write("override def putMember(key: String | Null, value: org.graalvm.polyglot.Value | Null): Unit = throw UnsupportedOperationException()\n");
    }

    protected override async generateInterfaceParameters(out: WritableStreamDefaultWriter<string>, node: ts.InterfaceDeclaration, localTypes: readonly string[]): Promise<void> {
        await out.write("(using context: _root_.dev.argon.plugin.loader.js.JSContext)")
    }

    protected override async generateInterface(out: WritableStreamDefaultWriter<string>, node: ts.InterfaceDeclaration): Promise<void> {
        await super.generateInterface(out, node);
        
        await this.writeIndent(out);
        await out.write("given ");
        await this.writeTypeParameters(out, [], node.typeParameters, ["_root_.dev.argon.plugin.loader.js.ValueEncoder", "_root_.dev.argon.plugin.loader.js.ValueDecoder"]);
        await out.write("(using context: _root_.dev.argon.plugin.loader.js.JSContext): _root_.dev.argon.plugin.loader.js.ValueEncoder[");
        await this.writeId(out, node.name);
        await this.writeTypeParametersAsArguments(out, node.typeParameters);
        await out.write("] with\n");

        await this.writeIndent(out, 1);
        await out.write("override def encode(value: ");
        await this.writeId(out, node.name);
        await this.writeTypeParametersAsArguments(out, node.typeParameters);
        await out.write("): _root_.org.graalvm.polyglot.Value = context.graalContext.asValue(value).nn\n");


        await this.writeIndent(out);
        await out.write("end given\n");
        
        await this.writeIndent(out);
        await out.write("given ");
        await this.writeTypeParameters(out, [], node.typeParameters, ["_root_.dev.argon.plugin.loader.js.ValueEncoder", "_root_.dev.argon.plugin.loader.js.ValueDecoder"]);
        await out.write("(using context: _root_.dev.argon.plugin.loader.js.JSContext): _root_.dev.argon.plugin.loader.js.ValueDecoder[");
        await this.writeId(out, node.name);
        await this.writeTypeParametersAsArguments(out, node.typeParameters);
        await out.write("] with\n");

        await this.writeIndent(out, 1);
        await out.write("override def decode(value: _root_.org.graalvm.polyglot.Value): ");
        await this.writeId(out, node.name);
        await this.writeTypeParametersAsArguments(out, node.typeParameters);
        await out.write(" = ???\n");


        await this.writeIndent(out);
        await out.write("end given\n");
    }
    
}



await new GraalJSGenerator().generate();
