import type { ModuleExportEntry, ModuleInfo, ModuleModel, ProgramModel } from "./program-model.js";
import { encodeTubePathComponent, ensureExhaustive, getModuleId, getModulePathExternalUrl, getModulePathUrl, modulePathEquals, tubePackageName, urlEncodeIdentifier } from "./util.js"

import type * as estree from "estree";
import type * as ir from "@argon-lang/js-backend-api/vm";
import { Identifier } from "@argon-lang/js-backend-api/vm";
import type { IterableElement, JsonObject, JsonValue, ReadonlyDeep } from "type-fest";
import { ExternFunction } from "./platform-data.js";
import type { ImportHandler } from "./imports.js";

import { name as isValidIdName } from "estree-util-is-identifier-name";

export interface OutputModuleInfo {
    readonly modulePath: ir.ModulePath;
    emitJsProgram(): ReadonlyDeep<estree.Program>;
}

export interface EmitOptions {
    readonly program: ProgramModel,
}

abstract class EmitterBase {
    constructor(
        readonly options: EmitOptions,
    ) {}



    /*
        Name encoding

        Full Export Name (used to reference types in signatures)
        Tube$dName$sModule$sName$s<export name>

        Export Name (identifier exported from module)
        global           - <name>$a<args>$r<result>$e
        synthetic nested - <parent>$k<index>

        Identifier
        <none>    - $_
        named     - the name, but with all $ encoded as $$ and any invalid characters URL encoded, using $XX
        binop     - The constructor name of the operator, but prefixed with $b and - replaced with _
        unop      - Same as binop, but using $u as a prefix
        extension - $x<name>
        inverse   - $i<name>
        update    - $m<name>


        Erased signature types
        builtin  - $z<int|bool|...>$a<args>$e
        function - $f<arg>$r<result>$e
        record   - $r<full export name>$a<args>$e
        tuple    - $t<items>$e
        erased   - $_

    */

    protected abstract getImportId(source: string): string;

    protected getFullExportName(importSpec: ir.ImportSpecifier): string {
        const moduleId = getModuleId(importSpec);

        const moduleInfo = this.options.program.getModuleInfo(moduleId);
        const tubeInfo = this.options.program.getTubeInfo(moduleInfo.tubeId);
        
        const tubePart = [ tubeInfo.tubeName.head, ...tubeInfo.tubeName.tail ]
            .map(s => this.getExportNameForId({ $type: "named", s }))
            .join("$d");

        const modulePart = moduleInfo.path.path
            .map(s => this.getExportNameForId({ $type: "named", s }) + "$s")
            .join("");

        return tubePart + "$s" + modulePart + this.getExportNameForImport(importSpec);
    }

    protected getExportNameForImport(importSpec: ir.ImportSpecifier): string {
        switch(importSpec.$type) {
            case "global":
                return this.getExportNameForId(importSpec.name) +
                    "$a" + importSpec.sig.params.map(arg => this.getExportNameForType(arg)).join("") +
                    "$r" + this.getExportNameForType(importSpec.sig.result);

            case "synthetic-nested":
                return this.getExportNameForImport(importSpec.parent) + "$k" + importSpec.index;
        }
    }

    protected getExportNameForId(id: Identifier | undefined): string {
        switch(id?.$type) {
            case undefined:
                return "$_";

            case "named":
                return urlEncodeIdentifier(id.s);

            case "bin-op":
                return "$b" + id.op.replaceAll("-", "_");

            case "un-op":
                return "$u" + id.op.replaceAll("-", "_");

            case "extension":
                return "$x" + this.getExportNameForId(id.inner);

            case "inverse":
                return "$i" + this.getExportNameForId(id.inner);

            case "update":
                return "$m" + this.getExportNameForId(id.inner);
        }
    }

    protected getExportNameForType(t: ir.ErasedSignatureType): string {
        switch(t.$type) {
            case "builtin":
            {
                const name = t.b.$type;
                const args = t.args.map(arg => this.getExportNameForType(arg)).join("");

                return "$b" + name + "$a" + args + "$e";
            }

            case "function":
                return "$f" + this.getExportNameForType(t.input) + "$r" + this.getExportNameForType(t.output) + "$e";

            case "record":
                return "$r" + this.getFullExportName(t.recordImport) +
                    "$a" + t.args.map(arg => this.getExportNameForType(arg)).join("") +
                    "$e";

            case "tuple":
                return "$t" + t.elements.map(elem => this.getExportNameForType(elem)).join("") +
                    "$e";

            case "erased":
                return "$_";
        }
    }



    protected getReg(r: ir.RegisterId): estree.Identifier {
        return {
            type: "Identifier",
            name: `r${r.id}`,
        };
    }

    protected getTypeParam(index: bigint): estree.Identifier {
        return {
            type: "Identifier",
            name: `t${index}`,
        };
    }

    protected getArgonRuntimeExport(name: string): estree.Expression {
        return {
            type: "MemberExpression",
            computed: false,
            optional: false,
            object: {
                type: "Identifier",
                name: this.getImportId("@argon-lang/runtime"),
            },
            property: {
                type: "Identifier",
                name,
            },
        };
    }
}

export function* emitTube(options: EmitOptions): Iterable<OutputModuleInfo> {
    for(const module of options.program.modules) {
        yield {
            modulePath: module.path,
            emitJsProgram() {
                const modEmitter = new ModuleEmitter(options, module);
                return modEmitter.emit();
            },
        };
    }
}

class ModuleEmitter extends EmitterBase implements ImportHandler {
    constructor(
        options: EmitOptions,
        private readonly module: ModuleModel,
    ) {
        super(options);
    }

    private readonly imports: string[] = [];
    private readonly moduleStatements: ReadonlyDeep<IterableElement<estree.Program["body"]>>[] = [];

    emit(): ReadonlyDeep<estree.Program> {
        for(const entry of this.module.exports) {
            this.emitEntry(entry);
        }

        const body: ReadonlyDeep<IterableElement<estree.Program["body"]>>[] = [];

        this.imports.forEach((path, i) => {
            body.push({
                type: "ImportDeclaration",
                specifiers: [
                    {
                        type: "ImportNamespaceSpecifier",
                        local: {
                            type: "Identifier",
                            name: "import" + i,
                        },
                    },
                ],
                source: {
                    type: "Literal",
                    value: path,
                },
                attributes: [],
            })
        });

        body.push(...this.moduleStatements);

        return {
            type: "Program",
            sourceType: "module",
            body,
        };
    }

    addDeclaration(declaration: ReadonlyDeep<estree.Declaration>): void {
        this.moduleStatements.push({
            type: "ExportNamedDeclaration",
            declaration,
            specifiers: [],
            attributes: [],
        });
    }

    override getImportId(source: string): string {
        let index = this.imports.indexOf(source);
        if(index < 0) {
            index = this.imports.length;
            this.imports.push(source);
        }

        return "import" + index;
    }



    private getImportSource(moduleInfo: ModuleInfo): string {
        if(moduleInfo.tubeId === 0n) {
            let hasRemoved = false;
            const parentModulePathParts = this.module.path.path.slice(0, Math.max(0, this.module.path.path.length - 1));
            const targetModulePathParts = moduleInfo.path.path.slice();

            while(parentModulePathParts.length > 0 && targetModulePathParts.length > 1 && parentModulePathParts[0] === targetModulePathParts[0]) {
                hasRemoved = true;
                parentModulePathParts.shift();
                targetModulePathParts.shift();
            }

            let prefix: string;
            if(parentModulePathParts.length === 0) {
                if(targetModulePathParts.length === 0) {
                    // No segments could have been removed and we want to reach the root of the package.
                    // This means we go up one level.
                    return "../";
                }

                prefix = "./";
            }
            else {
                prefix = "../".repeat(parentModulePathParts.length);
            }

            if(hasRemoved) {
                return prefix + targetModulePathParts.map(encodeTubePathComponent).join("/") + ".js";
            }
            else {
                return prefix + getModulePathUrl({ path: targetModulePathParts });
            }
        }
        else {
            const tubeInfo = this.options.program.getTubeInfo(moduleInfo.tubeId);
            const tubePackage = tubePackageName(tubeInfo.tubeName);

            const modulePathUrl = getModulePathExternalUrl(moduleInfo.path);
            
            if(modulePathUrl === "") {
                return tubePackage;
            }

            return tubePackage + "/" + modulePathUrl;
        }
    }

    getImportExpr(importSpec: ir.ImportSpecifier): estree.Expression {
        const moduleId = getModuleId(importSpec);
        const exportName = this.getExportNameForImport(importSpec);

        const moduleInfo = this.options.program.getModuleInfo(moduleId);
        if(moduleInfo.tubeId === 0n && modulePathEquals(moduleInfo.path, this.module.path)) {
            return {
                type: "Identifier",
                name: exportName,
            };
        }

        const importSource = this.getImportSource(moduleInfo);
        const importId = this.getImportId(importSource);

        return {
            type: "MemberExpression",
            object: {
                type: "Identifier",
                name: importId,
            },
            property: {
                type: "Identifier",
                name: exportName,
            },
            computed: false,
            optional: false,
        };
    }

    private emitEntry(entry: ModuleExportEntry): void {
        switch(entry.$type) {
            case "function-definition":
                this.emitFunction(entry.definition);
                break;

            case "record-definition":
                this.emitRecord(entry.definition);
                break;

            case "enum-definition":
                this.emitEnum(entry.definition);
                break;

            default:
                ensureExhaustive(entry);
        }
    }

    private emitFunction(func: ir.FunctionDefinition): void {
        if(func.implementation === undefined) {
            throw new Error("Missing function implementation");
        }

        switch(func.implementation.$type) {
            case "vm-ir":
            {
                const params: estree.Pattern[] = [];

                for(const i of func.signature.typeParameters.keys()) {
                    params.push({
                        type: "Identifier",
                        name: `t${i}`,
                    });
                }

                for(const i of func.signature.parameters.keys()) {
                    params.push({
                        type: "Identifier",
                        name: `r${i}`,
                    });
                }

                const blockEmitter = new BlockEmitter(this, func.signature.parameters.length);
                blockEmitter.emitBlock(func.implementation.body.block);

                this.addDeclaration({
                    type: "FunctionDeclaration",
                    id: {
                        type: "Identifier",
                        name: this.getExportNameForImport(func.import),
                    },
                    params: params,
                    body: blockEmitter.toBlock(),
                });
                break;
            }
                
            
            case "extern":
                const externRes = ExternFunction.codec.decode(func.implementation.extern);
                if(!externRes.success) {
                    throw new Error("Could not decode extern: " + externRes.message + " " + JSON.stringify(externRes.path));
                }

                const extern = externRes.value;

                const funcExpr = ExternFunction.getExprForImports(extern, this);

                this.addDeclaration({
                    type: "VariableDeclaration",
                    kind: "const",
                    declarations: [
                        {
                            type: "VariableDeclarator",
                            id: {
                                type: "Identifier",
                                name: this.getExportNameForImport(func.import),
                            },
                            init: funcExpr,
                        }
                    ],
                });
                break;

            default:
                ensureExhaustive(func.implementation);
        }
    }

    private emitRecord(rec: ir.RecordDefinition): void {
        const name = this.getExportNameForImport(rec.import);

        this.addDeclaration({
            type: "VariableDeclaration",
            kind: "const",
            declarations: [
                {
                    type: "VariableDeclarator",
                    id: {
                        type: "Identifier",
                        name,
                    },
                    init: {
                        type: "CallExpression",
                        optional: false,
                        callee: this.getArgonRuntimeExport("createRecordType"),
                        arguments: [
                            jsonToExpression({
                                typeParameterCount: rec.signature.typeParameters.length,
                                fields: rec.fields.map(field => ({
                                    name: this.getExportNameForId(field.name),
                                    mutable: field.mutable,
                                })),
                            }),
                        ],
                    },
                },
            ],
        });
    }

    private emitEnum(enumDef: ir.EnumDefinition): void {
        const name = this.getExportNameForImport(enumDef.import);

        const variantsObj: JsonObject = Object.create(null);
        for(const variant of enumDef.variants) {
            variantsObj[this.getExportNameForId(variant.name)] = {
                argCount: variant.signature.parameters.length,
                fields: variant.fields.map(field => ({
                    name: this.getExportNameForId(field.name),
                    mutable: field.mutable,
                }))
            };
        }

        this.addDeclaration({
            type: "VariableDeclaration",
            kind: "const",
            declarations: [
                {
                    type: "VariableDeclarator",
                    id: {
                        type: "Identifier",
                        name,
                    },
                    init: {
                        type: "CallExpression",
                        optional: false,
                        callee: this.getArgonRuntimeExport("createEnumType"),
                        arguments: [
                            jsonToExpression({
                                typeParameterCount: enumDef.signature.typeParameters.length,
                                variants: variantsObj,
                            }),
                        ],
                    },
                },
            ],
        });
    }
}

class BlockEmitter extends EmitterBase {
    constructor(
        private moduleEmitter: ModuleEmitter,
        private varOffset: number,
    ) {
        super(moduleEmitter.options);
    }

    readonly stmts: estree.Statement[] = [];

    protected override getImportId(source: string): string {
        return this.moduleEmitter.getImportId(source);
    }

    private emitNestedBlock(block: ir.Block): estree.BlockStatement {
        const nestedEmitter = new BlockEmitter(this.moduleEmitter, this.varOffset);
        nestedEmitter.emitBlock(block);
        return nestedEmitter.toBlock();
    }

    emitBlock(block: ir.Block): void {
        for(const insn of block.instructions) {
            this.emitInstruction(insn);
        }
    }

    toBlock(): estree.BlockStatement {
        return {
            type: "BlockStatement",
            body: this.stmts,
        };
    }

    private emitInstruction(insn: ir.Instruction): void {
        const stmts = this.stmts;

        const assign = (dest: ir.RegisterId, value: estree.Expression) => {
            stmts.push({
                type: "ExpressionStatement",
                expression: {
                    type: "AssignmentExpression",
                    left: this.getReg(dest),
                    operator: "=",
                    right: value,
                }
            });
        };

        const functionOutput = (dest: ir.FunctionResult, value: estree.Expression) => {
            let stmt: estree.Statement;
            switch(dest.$type) {
                case "discard":
                    stmt = {
                        type: "ExpressionStatement",
                        expression: value,
                    };
                    break;

                case "register":
                    stmt = {
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            left: this.getReg(dest.id),
                            operator: "=",
                            right: value,
                        },
                    };
                    break;

                case "return-value":
                    stmt = {
                        type: "ReturnStatement",
                        argument: value,
                    };
                    break;
            }

            stmts.push(stmt);
        }

        switch(insn.$type) {
            case "box":
                assign(insn.dest, this.getReg(insn.value));
                break;

            case "builtin-unary":
            {
                const unary = (operator: estree.UnaryOperator) => {
                    assign(insn.dest, {
                        type: "UnaryExpression",
                        prefix: true,
                        operator,
                        argument: this.getReg(insn.a),
                    });
                };

                switch(insn.op) {
                    case "int-bit-not":
                        unary("~");
                        break;

                    case "int-negate":
                        unary("-");
                        break;

                    case "bool-not":
                        unary("!");
                        break;

                    default:
                        ensureExhaustive(insn);
                }
                break;
            }

            case "builtin-binary":
            {
                const binary = (operator: estree.BinaryOperator) => {
                    assign(insn.dest, {
                        type: "BinaryExpression",
                        left: this.getReg(insn.a),
                        operator,
                        right: this.getReg(insn.b)
                    });
                };

                switch(insn.op) {
                    case "int-add":
                    case "string-concat":
                        binary("+");
                        break;

                    case "int-sub":
                        binary("-");
                        break;

                    case "int-mul":
                        binary("*");
                        break;

                    case "int-bit-and":
                        binary("&");
                        break;

                    case "int-bit-or":
                        binary("|");
                        break;

                    case "int-bit-xor":
                        binary("^");
                        break;

                    case "int-bit-shift-left":
                        binary("<<");
                        break;

                    case "int-bit-shift-right":
                        binary(">>");
                        break;

                    case "int-eq":
                    case "string-eq":
                    case "bool-eq":
                        binary("===");
                        break;

                    case "int-ne":
                    case "string-ne":
                    case "bool-ne":
                        binary("!==");
                        break;

                    case "int-lt":
                        binary("<");
                        break;

                    case "int-le":
                        binary("<=");
                        break;

                    case "int-gt":
                        binary(">");
                        break;

                    case "int-ge":
                        binary(">=");
                        break;

                    default:
                        ensureExhaustive(insn);
                }
                break;
            }

            case "const-bool":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                });
                break;

            case "const-int":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                    bigint: insn.value.toString()
                });
                break;

            case "const-string":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                });
                break;

            case "declare-variable":
                stmts.push({
                    type: "VariableDeclaration",
                    kind: "let",
                    declarations: [
                        {
                            type: "VariableDeclarator",
                            id: {
                                type: "Identifier",
                                name: `r${this.varOffset}`,
                            }
                        }
                    ],
                });
                ++this.varOffset;
                break;

            case "enum-variant-literal":
            {
                const args: estree.Expression[] = [];
                for(const typeArg of insn.typeArgs) {
                    args.push(this.buildTypeInfo(typeArg));
                }

                for(const arg of insn.args) {
                    args.push(this.getReg(arg));
                }

                args.push({
                    type: "ObjectExpression",
                    properties: insn.fields.map(field => {
                        const fieldInfo = this.options.program.getRecordFieldInfo(field.fieldId);
                        const id = this.getExportNameForId(fieldInfo.name);
                        const isValid = isValidIdName(id);

                        return {
                            type: "Property",
                            computed: !isValid,
                            method: false,
                            shorthand: false,
                            kind: "init",
                            key: isValid ? {
                                type: "Identifier",
                                name: id,
                            } : {
                                type: "Literal",
                                value: id,
                            },
                            value: this.getReg(field.value),
                        };
                    }),
                });

                assign(insn.dest, {
                    type: "NewExpression",
                    callee: this.getVariantClass(insn.enumType, insn.variantId),
                    arguments: args,
                });

                break;
            }

            case "finally":
            {
                stmts.push({
                    type: "TryStatement",
                    block: this.emitNestedBlock(insn.action),
                    finalizer: this.emitNestedBlock(insn.ensuring),
                });
                break;
            }

            case "function-call":
            {
                const functionInfo = this.options.program.getFunctionInfo(insn.functionId);
                const funcExpr = this.moduleEmitter.getImportExpr(functionInfo.importSpecifier);

                const args: estree.Expression[] = [];
                for(const typeArg of insn.typeArgs) {
                    args.push(this.buildTypeInfo(typeArg));
                }

                for(const arg of insn.args) {
                    args.push(this.getReg(arg));
                }

                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: funcExpr,
                    arguments: args,
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "function-object-call":
            {
                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: this.getReg(insn.function),
                    arguments: [ this.getReg(insn.arg) ],
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "function-object-type-call":
            {
                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: this.getReg(insn.function),
                    arguments: [ this.buildTypeInfo(insn.arg) ],
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "function-object-erased-call":
            {
                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: this.getReg(insn.function),
                    arguments: [],
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "if-else":
                stmts.push({
                    type: "IfStatement",
                    test: this.getReg(insn.condition),
                    consequent: this.emitNestedBlock(insn.whenTrue),
                    alternate: this.emitNestedBlock(insn.whenFalse),
                });
                break;

            case "is-enum-variant":
                stmts.push({
                    type: "ExpressionStatement",
                    expression: {
                        type: "AssignmentExpression",
                        operator: "=",
                        left: this.getReg(insn.dest),
                        right: {
                            type: "BinaryExpression",
                            operator: "instanceof",
                            left: this.getReg(insn.value),
                            right: this.getVariantClass(insn.enumType, insn.variantId),
                        },
                    },
                });

                const body: estree.Statement[] = [];
                for(const [i, arg] of insn.args.entries()) {
                    body.push({
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: this.getReg(arg),
                            right: {
                                type: "MemberExpression",
                                computed: false,
                                optional: false,
                                object: this.getReg(insn.value),
                                property: {
                                    type: "Identifier",
                                    name: `args_${i}`,
                                },
                            }
                        }
                    });
                }
                for(const fieldExtractor of insn.fieldExtractors) {
                    const fieldInfo = this.options.program.getRecordFieldInfo(fieldExtractor.fieldId);

                    body.push({
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: this.getReg(fieldExtractor.r),
                            right: {
                                type: "MemberExpression",
                                computed: false,
                                optional: false,
                                object: this.getReg(insn.value),
                                property: {
                                    type: "Identifier",
                                    name: `field_${this.getExportNameForId(fieldInfo.name)}`,
                                },
                            }
                        }
                    });
                }

                stmts.push({
                    type: "IfStatement",
                    test: this.getReg(insn.dest),
                    consequent: {
                        type: "BlockStatement",
                        body,
                    },
                });
                break;

            case "load-type-info":
                assign(insn.dest, this.buildTypeInfo(insn.type));
                break;

            case "move":
                assign(insn.dest, this.getReg(insn.src));
                break;

            case "partially-applied-function":
            case "partially-applied-type-function":
            case "partially-applied-function-erased":
            {
                const argName: estree.Identifier = {
                    type: "Identifier",
                    name: "arg",
                };

                const args: estree.Expression[] = [];
                for(const typeArg of insn.typeArgs) {
                    args.push(this.buildTypeInfo(typeArg));
                }

                if(insn.$type === "partially-applied-type-function") {
                    args.push(argName);
                }

                const block: estree.Statement[] = [];

                for(const arg of insn.args) {
                    switch(arg.$type) {
                        case "mutable":
                            args.push(this.getReg(arg.r));
                            break;

                        case "value":
                        {
                            const name: estree.Identifier = {
                                type: "Identifier",
                                name: "capture" + block.length,
                            };

                            block.push({
                                type: "VariableDeclaration",
                                kind: "const",
                                declarations: [
                                    {
                                        type: "VariableDeclarator",
                                        id: name,
                                        init: this.getReg(arg.r),
                                    },
                                ],
                            });

                            args.push(name);
                            break;
                        }

                        default:
                            ensureExhaustive(arg);
                    }
                }

                if(insn.$type === "partially-applied-function") {
                    args.push(argName);
                }

                const functionInfo = this.options.program.getFunctionInfo(insn.functionId);
                const funcExpr = this.moduleEmitter.getImportExpr(functionInfo.importSpecifier);

                let params: estree.Pattern[] = [];
                if(insn.$type !== "partially-applied-function-erased") {
                    params.push(argName);
                }

                const lambda: estree.ArrowFunctionExpression = {
                    type: "ArrowFunctionExpression",
                    expression: true,
                    body: {
                        type: "CallExpression",
                        callee: funcExpr,
                        arguments: args,
                        optional: false,
                    },
                    params,
                };

                if(block.length === 0) {
                    assign(insn.dest, lambda);
                }
                else {
                    block.push({
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: this.getReg(insn.dest),
                            right: lambda,
                        },
                    });

                    stmts.push({
                        type: "BlockStatement",
                        body: block,
                    })
                }
                break;
            }

            case "record-field-load":
            {
                const fieldInfo = this.options.program.getRecordFieldInfo(insn.fieldId);
                assign(insn.dest, {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: this.getReg(insn.recordValue),
                    property: {
                        type: "Identifier",
                        name: "field_" + this.getExportNameForId(fieldInfo.name),
                    },
                });
                break;
            }

            case "record-field-store":
            {
                const fieldInfo = this.options.program.getRecordFieldInfo(insn.fieldId);
                stmts.push({
                    type: "ExpressionStatement",
                    expression: {
                        type: "AssignmentExpression",
                        operator: "=",
                        left: {
                            type: "MemberExpression",
                            computed: false,
                            optional: false,
                            object: this.getReg(insn.recordValue),
                            property: {
                                type: "Identifier",
                                name: "field_" + this.getExportNameForId(fieldInfo.name),
                            },
                        },
                        right: this.getReg(insn.fieldValue),
                    },
                });
                break;
            }

            case "record-literal":
                assign(insn.dest, {
                    type: "NewExpression",
                    callee: this.buildTypeInfo(insn.recordType),
                    arguments: [
                        {
                            type: "ObjectExpression",
                            properties: insn.fields.map(field => {
                                const fieldInfo = this.options.program.getRecordFieldInfo(field.fieldId);
                                const id = this.getExportNameForId(fieldInfo.name);
                                const isValid = isValidIdName(id);

                                return {
                                    type: "Property",
                                    computed: !isValid,
                                    method: false,
                                    shorthand: false,
                                    kind: "init",
                                    key: isValid ? {
                                        type: "Identifier",
                                        name: id,
                                    } : {
                                        type: "Literal",
                                        value: id,
                                    },
                                    value: this.getReg(field.value),
                                };
                            }),
                        },
                    ],
                });
                break;

            case "return":
                stmts.push({
                    type: "ReturnStatement",
                    argument: this.getReg(insn.src),
                });
                break;

            case "tuple":
                if(insn.values.length === 0) {
                    assign(insn.dest, {
                        type: "Identifier",
                        name: "undefined",
                    });
                }
                else {
                    assign(insn.dest, {
                        type: "ArrayExpression",
                        elements: insn.values.map(element => this.getReg(element)),
                    });
                }
                break;

            case "tuple-element":
                assign(insn.dest, {
                    type: "MemberExpression",
                    object: this.getReg(insn.src),
                    property: {
                        type: "Literal",
                        value: Number(insn.elementIndex),
                    },
                    computed: true,
                    optional: false,
                });
                break;

            case "unbox":
                assign(insn.dest, this.getReg(insn.value));
                break;

            default:
                ensureExhaustive(insn);
        }
    }

    private buildTypeInfo(t: ir.VmType): estree.Expression {
        switch(t.$type) {
            case "builtin":
                switch(t.b.$type) {
                    case "bool":
                        return {
                            type: "Literal",
                            value: "boolean",
                        };

                    case "int":
                        return {
                            type: "Literal",
                            value: "bigint",
                        };
                        
                    case "string":
                        return {
                            type: "Literal",
                            value: "string",
                        };

                    case "never":
                    case "conjunction":
                    case "disjunction":
                        throw new Error("Not implemented buildTypeInfo builtin " + t.b.$type);
                }

            case "function":
                return {
                    type: "NewExpression",
                    callee: this.getArgonRuntimeExport("FunctionType"),
                    arguments: [
                        this.buildTypeInfo(t.input),
                        this.buildTypeInfo(t.output),
                    ],
                };

            case "function-erased":
                return {
                    type: "NewExpression",
                    callee: this.getArgonRuntimeExport("FunctionType"),
                    arguments: [
                        this.buildTypeInfo(t.output),
                    ],
                };

            case "record":
            {
                const rec = this.options.program.getRecordInfo(t.recordId);
                if(t.args.length === 0) {
                    return this.moduleEmitter.getImportExpr(rec.importSpecifier);
                }

                return {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: this.moduleEmitter.getImportExpr(rec.importSpecifier),
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: t.args.map(arg => this.buildTypeInfo(arg)),
                };
            }

            case "enum":
            {
                const rec = this.options.program.getEnumInfo(t.enumId);
                if(t.args.length === 0) {
                    return this.moduleEmitter.getImportExpr(rec.importSpecifier);
                }

                return {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: this.moduleEmitter.getImportExpr(rec.importSpecifier),
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: t.args.map(arg => this.buildTypeInfo(arg)),
                };
            }

            case "tuple":
                return {
                    type: "ArrayExpression",
                    elements: t.elements.map(item => this.buildTypeInfo(item)),
                };

            case "type-parameter":
                return this.getTypeParam(t.index);

            case "type-info":
                throw new Error("Not implemented buildTypeInfo type-info");

            case "of-type-info":
                return this.getReg(t.r);

            case "boxed":
                return this.getArgonRuntimeExport("erasedType");

            case "erased":
                throw new Error("Cannot get type info for erased");
        }
    }

    private getVariantClass(enumType: ir.VmType, variantId: bigint): estree.Expression {
        const variantInfo = this.options.program.getEnumVariantInfo(variantId);


        const variantName = this.getExportNameForId(variantInfo.name);
        const nameValid = isValidIdName(variantName) && variantName !== "__proto__";

        return {
            type: "MemberExpression",
            computed: !nameValid,
            optional: false,

            object: {
                type: "MemberExpression",
                computed: false,
                optional: false,

                object: this.buildTypeInfo(enumType),
                property: {
                    type: "Identifier",
                    name: "variants",
                },
            },

            property: nameValid ? {
                type: "Identifier",
                name: variantName,
            } : {
                type: "Literal",
                value: variantName,
            },
        };
    }
}

function jsonToExpression(expr: JsonValue): ReadonlyDeep<estree.Expression> {
    switch(typeof expr) {
        case "string":
        case "number":
        case "boolean":
            return {
                type: "Literal",
                value: expr,
            };

        case "object":
            if(expr === null) {
                return {
                    type: "Literal",
                    value: null,
                };
            }
            else if(expr instanceof Array) {
                return {
                    type: "ArrayExpression",
                    elements: expr.map(jsonToExpression),
                };
            }
            else {
                const properties: ReadonlyDeep<estree.Property>[] = [];
                for(const key of Object.keys(expr)) {
                    const value = jsonToExpression(expr[key]!);
                    if(key === "__proto__" || !isValidIdName(key)) {
                        properties.push({
                            type: "Property",
                            computed: true,
                            shorthand: false,
                            method: false,
                            kind: "init",
                            key: {
                                type: "Literal",
                                value: key,
                            },
                            value,
                        });
                    }
                    else {
                        properties.push({
                            type: "Property",
                            computed: false,
                            shorthand: false,
                            method: false,
                            kind: "init",
                            key: {
                                type: "Identifier",
                                name: key,
                            },
                            value,
                        });
                    }
                }
                return {
                    type: "ObjectExpression",
                    properties,
                };
            }

        default:
            ensureExhaustive(expr);
    }
}


