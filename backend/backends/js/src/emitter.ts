import type { ModuleExportEntry, ModuleInfo, ModuleModel, ProgramModel } from "./program-model.js";
import { encodeTubePathComponent, ensureExhaustive, getModuleId, getModulePathExternalUrl, getModulePathUrl, modulePathEquals, tubePackageName, urlEncodeIdentifier } from "./util.js"

import type * as estree from "estree";
import type * as ir from "@argon-lang/js-backend-api/vm";
import { Identifier } from "@argon-lang/js-backend-api/vm";
import type { IterableElement, ReadonlyDeep } from "type-fest";
import { ExternFunction } from "./platform-data.js";
import type { ImportHandler } from "./imports.js";

export interface OutputModuleInfo {
    readonly modulePath: ir.ModulePath;
    emitJsProgram(): ReadonlyDeep<estree.Program>;
}

export interface EmitOptions {
    readonly program: ProgramModel,
}

class EmitterBase {
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
}

export class TubeEmitter extends EmitterBase {
    constructor(options: EmitOptions) {
        super(options);
    }

    *emit(): Iterable<OutputModuleInfo> {
        const options = this.options;
        for(const module of this.options.program.modules) {
            yield {
                modulePath: module.path,
                emitJsProgram() {
                    const modEmitter = new ModuleEmitter(options, module);
                    return modEmitter.emit();
                },
            };
        }
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
        });
    }

    getImportId(source: string): string {
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
        const params: estree.Pattern[] = [];
        const body: estree.Statement[] = [];

        for(const field of rec.fields) {
            const fieldName = "field_" + this.getExportNameForId(field.name);
            params.push({
                type: "Identifier",
                name: fieldName,
            });

            body.push({
                type: "ExpressionStatement",
                expression: {
                    type: "AssignmentExpression",
                    operator: "=",
                    left: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: {
                            type: "ThisExpression",
                        },
                        property: {
                            type: "Identifier",
                            name: fieldName,
                        },
                    },
                    right: {
                        type: "Identifier",
                        name: fieldName,
                    },
                }
            });
        }

        this.addDeclaration({
            type: "FunctionDeclaration",
            id: {
                type: "Identifier",
                name,
            },
            params,
            body: {
                type: "BlockStatement",
                body,
            },
        });

        this.moduleStatements.push({
            type: "ExpressionStatement",
            expression: {
                type: "AssignmentExpression",
                operator: "=",
                left: {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: {
                        type: "Identifier",
                        name,
                    },
                    property: {
                        type: "Identifier",
                        name: "specialize",
                    }
                },
                right: {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: {
                        type: "Identifier",
                        name: this.getImportId("@argon-lang/runtime"),
                    },
                    property: {
                        type: "Identifier",
                        name: "specialize",
                    },
                },
            },
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

        switch(insn.$type) {
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
                        binary("===");
                        break;

                    case "int-ne":
                    case "string-ne":
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

                let stmt: estree.Statement;
                switch(insn.dest.$type) {
                    case "discard":
                        stmt = {
                            type: "ExpressionStatement",
                            expression: callExpr,
                        };
                        break;

                    case "register":
                        stmt = {
                            type: "ExpressionStatement",
                            expression: {
                                type: "AssignmentExpression",
                                left: this.getReg(insn.dest.id),
                                operator: "=",
                                right: callExpr,
                            },
                        };
                        break;

                    case "return-value":
                        stmt = {
                            type: "ReturnStatement",
                            argument: callExpr,
                        };
                        break;
                }

                stmts.push(stmt);
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

            case "load-type-info":
                assign(insn.dest, this.buildTypeInfo(insn.type));
                break;

            case "move":
                assign(insn.dest, this.getReg(insn.src));
                break;

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
                    arguments: insn.fields.map(field => this.getReg(field.value)),
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
                throw new Error("Not implemented buildTypeInfo function");

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

            case "erased":
                throw new Error("Cannot get type info for erased");
        }
    }
}


