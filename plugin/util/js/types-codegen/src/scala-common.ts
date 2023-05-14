import ts from "typescript";
import * as path from "node:path";
import * as fs from "node:fs/promises";
import { CodeGenerator } from "./common.js";


export abstract class ScalaGenerator extends CodeGenerator {

    #indentLevel: number = 0;

    indent(): void {
        ++this.#indentLevel;
    }

    unindent(): void {
        --this.#indentLevel;
    }

    async writeIndent(out: WritableStreamDefaultWriter<string>, offset: number = 0): Promise<void> {
        for(let i = 0; i < (this.#indentLevel + offset); ++i) {
            await out.write("\t");
        }
    }

    async writeId(out: WritableStreamDefaultWriter<string>, id: ts.Identifier): Promise<void> {
        let name = this.getIdName(id);
        switch(name) {
            case "abstract":
            case "sealed":
            case "final":
            case "override":
                name = `\`${name}\``;
                break;
        }

        await out.write(name);
    }

    async writeProp(out: WritableStreamDefaultWriter<string>, id: ts.PropertyName): Promise<void> {
        if(ts.isIdentifier(id)) {
            await this.writeId(out, id);
        }
        else {
            throw new Error("Unsupported identifier");
        }
    }

    async writeBindingName(out: WritableStreamDefaultWriter<string>, id: ts.BindingName): Promise<void> {
        if(ts.isIdentifier(id)) {
            await this.writeId(out, id);
        }
        else {
            throw new Error("Unsupported identifier");
        }
    }

    #nameMatches(typeName: ts.EntityName, name: string): boolean {
        return ts.isIdentifier(typeName) && this.getIdName(typeName) === name;
    }

    #leftHandSideToTypeName(lhs: ts.LeftHandSideExpression): ts.EntityName {
        if(ts.isIdentifier(lhs)) {
            return lhs;
        }
        else {
            throw new Error("Unexpected left hand side node");
        }
    }

    protected abstract writePromiseType(out: WritableStreamDefaultWriter<string>): Promise<void>;
    protected abstract writeMapType(out: WritableStreamDefaultWriter<string>): Promise<void>;
    protected abstract writeUint8ArrayType(out: WritableStreamDefaultWriter<string>): Promise<void>;
    protected abstract writeBigIntType(out: WritableStreamDefaultWriter<string>): Promise<void>;
    protected abstract writeArrayType(out: WritableStreamDefaultWriter<string>): Promise<void>;
    protected abstract writeFunctionType(out: WritableStreamDefaultWriter<string>, t: ts.FunctionTypeNode, localTypes: readonly string[]): Promise<void>;


    async writeType(out: WritableStreamDefaultWriter<string>, t: ts.TypeNode, localTypes: readonly string[]): Promise<void> {
        if(ts.isTypeReferenceNode(t) || ts.isExpressionWithTypeArguments(t)) {
            const typeName = ts.isTypeReferenceNode(t) ? t.typeName : this.#leftHandSideToTypeName(t.expression);

            if(this.#nameMatches(typeName, "Promise")) {
                await this.writePromiseType(out);
            }
            else if(this.#nameMatches(typeName, "Map")) {
                await this.writeMapType(out);
            }
            else if(this.#nameMatches(typeName, "AsyncIterable")) {
                await out.write("_root_.dev.argon.plugin.loader.js.JSAsyncIterable");
            }
            else if(this.#nameMatches(typeName, "Uint8Array")) {
                await this.writeUint8ArrayType(out);
            }
            else {
                const writeEntityName = async (name: ts.EntityName) => {
                    if(ts.isQualifiedName(name)) {
                        await writeEntityName(name.left);
                        await out.write(".");
                        await this.writeId(out, name.right);
                    }
                    else {
                        const typeName = this.getIdName(name);
        
                        if(localTypes.indexOf(typeName) >= 0) {
                            await out.write(typeName);
                        }
                        else {
                            const importInfo = this.lookupName(typeName);
                            if(importInfo === undefined) {
                                throw new Error(`Unknown type: ${typeName}`);
                            }
                            
                            await out.write("_root_.");
                            await out.write(this.getPackageForFile(importInfo.path))
                            if(!importInfo.isModule) {
                                await out.write(".");
                                await out.write(importInfo.realName);
                            }
                        }
                    }
                };

                await writeEntityName(typeName);
            }

            if(t.typeArguments !== undefined && t.typeArguments.length > 0) {
                await out.write("[");
                for(const [i, arg] of t.typeArguments.entries()) {
                    await this.writeType(out, arg, localTypes);
                    if(i < t.typeArguments.length - 1) {
                        await out.write(", ");
                    }
                }
                await out.write("]");
            }
        }
        else if(ts.isArrayTypeNode(t)) {
            await this.writeArrayType(out);
            await out.write("[");
            await this.writeType(out, t.elementType, localTypes);
            await out.write("]");
        }
        else if(ts.isFunctionTypeNode(t)) {
            await this.writeFunctionType(out, t, localTypes);
        }
        else if(t.kind === ts.SyntaxKind.StringKeyword) {
            await out.write("_root_.java.lang.String");
        }
        else if(t.kind === ts.SyntaxKind.BigIntKeyword) {
            await this.writeBigIntType(out);
        }
        else if(t.kind === ts.SyntaxKind.VoidKeyword) {
            await out.write("_root_.scala.Unit");
        }
        else if(ts.isLiteralTypeNode(t)) {
            if(t.literal.kind === ts.SyntaxKind.NullKeyword) {
                await out.write("_root_.scala.Null");
            }
            else if(t.literal.kind === ts.SyntaxKind.TrueKeyword) {
                await out.write("true");
            }
            else if(t.literal.kind === ts.SyntaxKind.FalseKeyword) {
                await out.write("false");
            }
            else if(ts.isStringLiteral(t.literal)) {
                await out.write(`"${t.literal.text}"`);
            }
            else {
                console.log(t.literal);
                throw new Error("Unsupported literal type");
            }
        }
        else if(ts.isUnionTypeNode(t)) {
            for(const [i, element] of t.types.entries()) {
                await this.writeType(out, element, localTypes);
                if(i < t.types.length - 1) {
                    await out.write(" | ");
                }
            }
        }
        else if(ts.isTypeOperatorNode(t) && t.operator === ts.SyntaxKind.ReadonlyKeyword) {
            await this.writeType(out, t.type, localTypes);
        }
        else if(ts.isParenthesizedTypeNode(t)) {
            await out.write("(");
            await this.writeType(out, t.type, localTypes);
            await out.write(")");
        }
        else {
            console.log(t);
            throw new Error("Unsupported type");
        }
    }

    async writeTypeParameters(out: WritableStreamDefaultWriter<string>, localTypes: readonly string[], typeParams: readonly ts.TypeParameterDeclaration[] | undefined, contextBounds: readonly string[]) {
        if(typeParams !== undefined && typeParams.length > 0) {
            await out.write("[");
            const localTypes2 = [ ...localTypes, ...typeParams.map(p => this.getIdName(p.name)) ];
            for(const [i, typeParam] of typeParams.entries()) {
                await this.writeId(out, typeParam.name);
    
                if(typeParam.constraint !== undefined) {
                    await out.write(" <: ");
                    await this.writeType(out, typeParam.constraint, localTypes2);
                }

                for(const contextBound of contextBounds) {
                    await out.write(": ");
                    await out.write(contextBound);
                }
    
                if(i < typeParams.length - 1) {
                    await out.write(", ");
                }
            }
            await out.write("]");
        }
    }
    
    protected override async generateStart(out: WritableStreamDefaultWriter<string>): Promise<void> {
        await out.write(`package ${this.getPackageForFile(this.inputFile)}\n`);
    }

    protected abstract readonly baseType: string;
    protected abstract readonly contextBounds: readonly string[];

    protected async generateAdditionalInterfaceMethods(out: WritableStreamDefaultWriter<string>, node: ts.InterfaceDeclaration, localTypes: readonly string[]): Promise<void> {}
    protected async generateInterfaceParameters(out: WritableStreamDefaultWriter<string>, node: ts.InterfaceDeclaration, localTypes: readonly string[]): Promise<void> {}

    protected override async generateInterface(out: WritableStreamDefaultWriter<string>, node: ts.InterfaceDeclaration): Promise<void> {
        await this.writeIndent(out);
        await out.write("trait ");
        await this.writeId(out, node.name);
        await this.writeTypeParameters(out, [], node.typeParameters, this.contextBounds);
        const localTypes = node.typeParameters?.map(p => this.getIdName(p.name)) ?? [];
        await this.generateInterfaceParameters(out, node, localTypes);
        await out.write(" extends ");
        await out.write(this.baseType);
        if(node.heritageClauses !== undefined && node.heritageClauses.length > 0) {
            for(const superType of node.heritageClauses.flatMap(clause => clause.types)) {
                await out.write(" with ");
                await this.writeType(out, superType, localTypes);
            }
        }
        await out.write(" {\n");
        for(const member of node.members) {
            if(ts.isMethodSignature(member)) {
                await this.writeIndent(out, 1);
                await out.write("def ");
                await this.writeProp(out, member.name);
                await this.writeTypeParameters(out, localTypes, member.typeParameters, this.contextBounds);

                const localTypes2 = [ ...localTypes, ...member.typeParameters?.map(p => this.getIdName(p.name)) ?? [] ];

                await out.write("(");
                for(const [i, param] of member.parameters.entries()) {
                    if(param.type === undefined) {
                        throw new Error("Missing type on parameter");
                    }


                    await this.writeBindingName(out, param.name);
                    await out.write(": ");
                    await this.writeType(out, param.type, localTypes2);
                    if(i < member.parameters.length - 1) {
                        await out.write(", ");
                    }
                }
                await out.write("): ");
                
                if(member.type === undefined) {
                    throw new Error("Missing return type");
                }
                await this.writeType(out, member.type, localTypes2);

                await out.write("\n");
            }
            else if(ts.isPropertySignature(member)) {
                await this.writeIndent(out, 1);
                await out.write("val ");
                await this.writeProp(out, member.name);
                await out.write(": ");
                
                if(member.type === undefined) {
                    throw new Error("Missing property type");
                }
                await this.writeType(out, member.type, localTypes);

                await out.write("\n");
            }
            else if(ts.isGetAccessor(member)) {
                await this.writeIndent(out, 1);
                await out.write("def ");
                await this.writeProp(out, member.name);
                await out.write(": ");
                
                if(member.type === undefined) {
                    throw new Error("Missing property type");
                }
                await this.writeType(out, member.type, localTypes);

                await out.write("\n");
            }
            else {
                console.log(member);
                throw new Error("Unexpected member");
            }
        }

        await this.generateAdditionalInterfaceMethods(out, node, localTypes);

        await this.writeIndent(out);
        await out.write("}\n");
    }

    protected override async generateTypeAlias(out: WritableStreamDefaultWriter<string>, node: ts.TypeAliasDeclaration): Promise<void> {
        await this.writeIndent(out);
        await out.write("type ");
        await this.writeId(out, node.name);
        await this.writeTypeParameters(out, [], node.typeParameters, []);
        await out.write(" = ");
        const localTypes = node.typeParameters?.map(p => this.getIdName(p.name)) ?? [];
        await this.writeType(out, node.type, localTypes);
        await out.write("\n");
    }
    
    protected override async generateNamespace(out: WritableStreamDefaultWriter<string>, node: ts.ModuleDeclaration): Promise<void> {
        if(!ts.isIdentifier(node.name)) {
            throw new Error("Invalid module name");
        }

        if(node.body === undefined || !ts.isModuleBlock(node.body)) {
            throw new Error("Invalid module body");
        }

        await this.writeIndent(out);
        await out.write("object ");
        await this.writeId(out, node.name);
        await out.write(" {\n")

        this.indent();

        for(const elem of node.body.statements) {
            await this.generateNode(out, elem);
        }

        this.unindent();

        await this.writeIndent(out);
        await out.write("}\n")
    }

    protected override async generateEnd(out: WritableStreamDefaultWriter<string>): Promise<void> {
        
    }
    
}

