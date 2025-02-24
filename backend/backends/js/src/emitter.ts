import type { ModuleExportEntry, ModuleInfo, ModuleModel, ProgramModel } from "./program-model.js";
import { encodeTubePathComponent, ensureExhaustive, getModulePathExternalUrl, getModulePathUrl, modulePathEquals, tubeNameEquals, tubePackageName, urlEncodeIdentifier } from "./util.js"

import type * as estree from "estree";
import type * as ir from "@argon-lang/js-backend-api/vm";
import type { Identifier } from "@argon-lang/js-backend-api/vm";
import { type ExternProvider } from "./externs.js";
import type { IterableElement, ReadonlyDeep } from "type-fest";
import { ExternFunction } from "./platform-data.js";
import type { ImportHandler } from "./imports.js";

export interface OutputModuleInfo {
    readonly modulePath: ir.ModulePath;
    emitJsProgram(): ReadonlyDeep<estree.Program>;
}

export interface EmitOptions {
    readonly program: ProgramModel,
    readonly externProvider: ExternProvider;
    readonly tubeMapping: readonly TubeMapping[];
}

export interface TubeMapping {
    readonly tubeName: ir.TubeName;
    readonly packageName: string;
}

class EmitterBase {
    constructor(
        readonly options: EmitOptions,
    ) {}



    /*
        Name encoding

        Import Specifier
        Tube$dName$sModule$sName$s<name><sig>

        Name + Sig
        <name>$a<args>$r<result>$e

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
        record   - $r<import>$a<args>$e
        tuple    - $t<items>$e
        erased   - $_

    */


    protected getExportNameForImportSpecifier(importSpec: ir.ImportSpecifier): string {
        const moduleInfo = this.options.program.getModuleInfo(importSpec.moduleId);
        const tubeInfo = this.options.program.getTubeInfo(moduleInfo.tubeId);
        
        const tubePart = [ tubeInfo.tubeName.head, ...tubeInfo.tubeName.tail ]
            .map(s => this.getExportNameForId({ $type: "named", s }))
            .join("$d");

        const modulePart = moduleInfo.path.path
            .map(s => this.getExportNameForId({ $type: "named", s }) + "$s")
            .join("");

        return tubePart + "$s" + modulePart + this.getExportNameForIdSig(importSpec);
    }

    protected getExportNameForIdSig(importSpec: Pick<ir.ImportSpecifier, "name" | "sig">): string {
        return this.getExportNameForId(importSpec.name) +
            "$a" + importSpec.sig.params.map(arg => this.getExportNameForType(arg)).join("") +
            "$r" + this.getExportNameForType(importSpec.sig.result);
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
                return "$r" + this.getExportNameForImportSpecifier(t.recordImport) +
                    "$a" + t.args.map(arg => this.getExportNameForType(arg)).join("") +
                    "$e";

            case "tuple":
                return "$t" + t.elements.map(elem => this.getExportNameForType(elem)).join("") +
                    "$e";

            case "erased":
                return "$_";
        }
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
    private readonly declarations: ReadonlyDeep<estree.Declaration>[] = [];

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

        for(const declaration of this.declarations) {
            body.push({
                type: "ExportNamedDeclaration",
                declaration,
                specifiers: [],
            });
        }

        return {
            type: "Program",
            sourceType: "module",
            body,
        };
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
            let tubePackage = this.options.tubeMapping.find(mapping => tubeNameEquals(mapping.tubeName, tubeInfo.tubeName))?.packageName;
            if(tubePackage === undefined) {
                tubePackage = tubePackageName(tubeInfo.tubeName);
            }

            const modulePathUrl = getModulePathExternalUrl(moduleInfo.path);
            
            if(modulePathUrl === "") {
                return tubePackage;
            }

            return tubePackage + "/" + modulePathUrl;
        }
    }

    private getImportExpr(importSpec: ir.ImportSpecifier): estree.Expression {
        const exportName = this.getExportNameForIdSig(importSpec);

        const moduleInfo = this.options.program.getModuleInfo(importSpec.moduleId);
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
                this.declarations.push({
                    type: "FunctionDeclaration",
                    id: {
                        type: "Identifier",
                        name: this.getExportNameForIdSig(func.import),
                    },
                    params: func.signature.parameters.map((_, i) => ({
                        type: "Identifier",
                        name: `r${i}`,
                    })),
                    body: this.emitFunctionBody(func.implementation.body, func.signature.parameters.length),
                });
                break;
            
            case "extern":
                const extern = this.options.externProvider.getExternFunction(func.implementation.name);
                if(extern === undefined) {
                    throw new Error("Unknown extern: " + func.implementation.name);
                }

                const funcExpr = ExternFunction.getExprForImports(extern, this);

                this.declarations.push({
                    type: "VariableDeclaration",
                    kind: "const",
                    declarations: [
                        {
                            type: "VariableDeclarator",
                            id: {
                                type: "Identifier",
                                name: this.getExportNameForIdSig(func.import),
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

    private emitRecord(_rec: ir.RecordDefinition): void {
        throw new Error("Not implemented");
    }


    private emitFunctionBody(body: ir.FunctionBody, varOffset: number): estree.BlockStatement {
        const stmts: estree.Statement[] = [];

        for(let i = 0; i < body.variables.length; ++i) {
            stmts.push({
                type: "VariableDeclaration",
                kind: "let",
                declarations: [
                    {
                        type: "VariableDeclarator",
                        id: {
                            type: "Identifier",
                            name: `r${i + varOffset}`,
                        },
                    },
                ],
            });
        }

        this.emitBlockWith(body.block, stmts);

        return {
            type: "BlockStatement",
            body: stmts,
        }
    }

    private emitBlockWith(block: ir.Block, stmts: estree.Statement[]): void {
        for(const insn of block.instructions) {
            this.emitInstruction(insn, stmts);
        }
    }

    private emitBlock(block: ir.Block): estree.BlockStatement {
        const stmts: estree.Statement[] = [];
        this.emitBlockWith(block, stmts);
        return {
            type: "BlockStatement",
            body: stmts,
        };
    }

    private emitInstruction(insn: ir.Instruction, stmts: estree.Statement[]): void {

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

            case "function-call":
            {
                const functionInfo = this.options.program.getFunctionInfo(insn.functionId);
                const funcExpr = this.getImportExpr(functionInfo.importSpecifier);

                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: funcExpr,
                    arguments: insn.args.map(arg => this.getReg(arg)),
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
                    consequent: this.emitBlock(insn.whenTrue),
                    alternate: this.emitBlock(insn.whenFalse),
                });
                break;

            case "load-type-info":
                assign(insn.dest, this.buildTypeInfo(insn.type));
                break;

            case "move":
                assign(insn.dest, this.getReg(insn.src));
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
                            type: "MemberExpression",
                            object: {
                                type: "Identifier",
                                name: "globalThis",
                            },
                            property: {
                                type: "Identifier",
                                name: "Boolean"
                            },
                            computed: false,
                            optional: false,
                        };

                    case "int":
                        return {
                            type: "MemberExpression",
                            object: {
                                type: "Identifier",
                                name: "globalThis",
                            },
                            property: {
                                type: "Identifier",
                                name: "BigInt"
                            },
                            computed: false,
                            optional: false,
                        };
                        
                    case "string":
                        return {
                            type: "MemberExpression",
                            object: {
                                type: "Identifier",
                                name: "globalThis",
                            },
                            property: {
                                type: "Identifier",
                                name: "String"
                            },
                            computed: false,
                            optional: false,
                        };

                    case "never":
                    case "conjunction":
                    case "disjunction":
                        throw new Error("Not implemented buildTypeInfo builtin " + t.b.$type);
                }

            case "function":
                throw new Error("Not implemented buildTypeInfo function");

            case "record":
                throw new Error("Not implemented buildTypeInfo record");

            case "tuple":
                return {
                    type: "ArrayExpression",
                    elements: t.elements.map(item => this.buildTypeInfo(item)),
                };

            case "type-info":
                throw new Error("Not implemented buildTypeInfo type-info");
        }
    }

    private getReg(r: ir.RegisterId): estree.Identifier {
        return {
            type: "Identifier",
            name: `r${r.id}`,
        };
    }
}


