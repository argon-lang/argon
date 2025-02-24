import { ESExpr, type ESExprCodec } from "@argon-lang/esexpr";
import * as esexpr from "@argon-lang/esexpr";
import { ensureExhaustive } from "./util.js";

import type * as estree from "estree";
import type { ReadonlyDeep } from "type-fest";
import type { ESExprTag } from "@argon-lang/esexpr";
import { name as isValidName } from "estree-util-is-identifier-name";

export interface ExternProvider {
    getExternFunction(name: string): ExternFunction;
}


export class ExternLoader implements ExternProvider {

    #externs = new Map<string, ExternBuilder>();

    get externNames(): Iterable<string> {
        return this.#externs.keys();
    }

    addExterns(node: estree.Program): void {
        const moduleExternLoader = new ModuleExternLoader();


        for(const extern of moduleExternLoader.load(node)) {
            if(this.#externs.has(extern.name)) {
                throw new Error("Duplicate extern: " + extern.name);
            }

            this.#externs.set(extern.name, extern.extern);
        }
    }

    getExternFunction(name: string): ExternFunction {
        const extern = this.#externs.get(name);
        if(extern === undefined) {
            throw new Error("Unknown extern: " + name);
        }

        return extern.buildExtern();
    }
}

class ModuleExternLoader {
    private readonly importedIds = new Map<string, ImportedId>();

    * load(node: ReadonlyDeep<estree.Program>): Iterable<NamedExtern> {
        if(node.sourceType !== "module") {
            throw new Error("Externs must be defined in an ES Module");
        }

        for(const stmt of node.body) {
            switch(stmt.type) {
                case "ImportDeclaration":
                    this.addImport(stmt);
                    break;

                case "ExportNamedDeclaration":
                    yield this.addExport(stmt);
                    break;

                default:
                    throw new Error("Only import and export statements are allowed in module used for externs.");
            }
        }
    }

    private addImport(importDecl: ReadonlyDeep<estree.ImportDeclaration>): void {
        if(typeof importDecl.source.value !== "string") {
            throw new Error("Import source must be a string literal.");
        }

        for(const specifier of importDecl.specifiers) {
            if(this.importedIds.has(specifier.local.name)) {
                throw new Error("Duplicate import name: " + specifier.local.name);
            }

            let member: string | undefined;

            switch(specifier.type) {
                case "ImportNamespaceSpecifier":
                    member = undefined;
                    break;

                case "ImportDefaultSpecifier":
                    member = "default";
                    break;

                case "ImportSpecifier":
                    if(specifier.imported.type === "Identifier") {
                        member = specifier.imported.name;
                    }
                    else if(typeof specifier.imported.value === "string") {
                        member = specifier.imported.value;
                    }
                    else {
                        throw new Error("Imported must be an identifier or string literal.");
                    }
                    break;
                
                default:
                    ensureExhaustive(specifier);
            }


            const importedId: ImportedId = {
                localAlias: specifier.local.name,
                source: importDecl.source.value,
                member,
            };

            this.importedIds.set(specifier.local.name, importedId);
        }
    }

    private addExport(exportDecl: ReadonlyDeep<estree.ExportNamedDeclaration>): NamedExtern {
        if(!exportDecl.declaration || exportDecl.declaration.type !== "FunctionDeclaration") {
            throw new Error("Externs must be defined by exporting a function definition");
        }

        const modExternLoader = this;

        const decl = exportDecl.declaration;
        return {
            name: exportDecl.declaration.id.name,
            extern: {
                buildExtern() {
                    return modExternLoader.buildExternFunction(decl);
                },
            },
        };
    }

    private buildExternFunction(func: ReadonlyDeep<estree.FunctionDeclaration>): ExternFunction {
        const freeVars = new Set<string>();
        const scanner = new FreeVariableScanner(new Set(), freeVars);
        scanner.scan(func);

        const referencedImports: ImportedId[] = [];

        for(const freeVar of freeVars) {
            const importedId = this.importedIds.get(freeVar);
            if(importedId === undefined) {
                continue;
            }
            
            referencedImports.push(importedId);
        }

        return {
            function: func,
            referencedImports,
        };
    }
}

interface NamedExtern {
    readonly name: string;
    readonly extern: ExternBuilder;
}

interface ExternBuilder {
    buildExtern(): ExternFunction;
}

export interface ExternFunction {
    readonly function: ReadonlyDeep<estree.FunctionDeclaration>;
    readonly referencedImports: readonly ImportedId[];
}

export namespace ExternFunction {
    export const codec: ESExprCodec<ExternFunction> = esexpr.lazyCodec(() =>
        esexpr.recordCodec<ExternFunction>("estree", {
            function: esexpr.positionalFieldCodec(EstreeRepr.codecOfNode<estree.FunctionDeclaration>()),
            referencedImports: esexpr.keywordFieldCodec("referenced-imports", esexpr.listCodec(ImportedId.codec)),
        })
    );

    export function getExprForImports(extern: ExternFunction, importHandler: ImportHandler): ReadonlyDeep<estree.Expression> {
        const node = extern.function;
        const body: ReadonlyDeep<estree.Statement>[] = [];

        for(const importedId of extern.referencedImports) {
            const mappedId = importHandler.getImportId(importedId.source);

            let value: estree.Expression;
            if(importedId.member) {
                const memberIsValidId = isValidName(importedId.member);
                const property: estree.Expression = memberIsValidId
                    ? { type: "Identifier", name: importedId.member }
                    : { type: "Literal", value: importedId.member };

                value = {
                    type: "MemberExpression",
                    object: {
                        type: "Identifier",
                        name: mappedId,
                    },
                    property: property,
                    computed: false,
                    optional: false,
                };
            }
            else {
                value = {
                    type: "Identifier",
                    name: mappedId,
                };
            }

            body.push({
                type: "VariableDeclaration",
                kind: "const",
                declarations: [
                    {
                        type: "VariableDeclarator",
                        id: {
                            type: "Identifier",
                            name: importedId.localAlias,
                        },
                        init: value,
                    },
                ],
            });
        }

        const funcExpr: ReadonlyDeep<estree.FunctionExpression> = {
            type: "FunctionExpression",
            id: node.id,
            params: node.params,
            body: node.body,
            generator: node.generator,
            async: node.async,
        };
        
        if(body.length === 0) {
            return funcExpr;
        }
        else {
            body.push({
                type: "ReturnStatement",
                argument: funcExpr,
            });

            return {
                type: "CallExpression",
                callee: {
                    type: "FunctionExpression",
                    params: [],
                    body: {
                        type: "BlockStatement",
                        body,
                    },
                },
                arguments: [],
                optional: false,
            };
        }
    }
}


type EStreeRepr =
    | string
    | number
    | RegExp
    | bigint
    | boolean
    | null
    | readonly EStreeRepr[]
    | { readonly [k: string]: EStreeRepr | undefined }
;

namespace EstreeRepr {
    type PartialEstreeReprObject = { [k: string]: EStreeRepr | undefined };

    export function codecOfNode<Node extends estree.Node>(): ESExprCodec<ReadonlyDeep<Node>> {
        return codec as unknown as ESExprCodec<ReadonlyDeep<Node>>;
    }

    export const codec: ESExprCodec<EStreeRepr> = {
        get tags() { return new Set<ESExprTag>() },

        encode: function (value: EStreeRepr): ESExpr {
            switch(typeof value) {
                case "string":
                case "number":
                case "bigint":
                case "boolean":
                    return value;

                case "object":
                    if(value === null) {
                        return null;
                    }
                    else if(value instanceof RegExp) {
                        return {
                            type: "constructor",
                            name: "RegExp",
                            args: [
                                value.source,
                                value.flags,
                            ],
                            kwargs: new Map(),
                        };
                    }
                    else if(value instanceof Array) {
                        return esexpr.listCodec(this).encode(value);
                    }
                    else if(typeof value["type"] === "string") {
                        return {
                            type: "constructor",
                            name: value["type"],
                            args: [],
                            kwargs: new Map(
                                Object.entries(value)
                                    .map<[string, ESExpr] | null>(([name, value]) => {
                                        if(name === "type" || value === undefined) {
                                            return null;
                                        }
                                        return [name, this.encode(value)];
                                    })
                                    .filter<[string, ESExpr]>(pair => pair !== null)
                            ),
                        };
                    }
                    else {
                        return {
                            type: "constructor",
                            name: "obj",
                            args: [],
                            kwargs: new Map(
                                Object.entries(value)
                                    .map<[string, ESExpr] | null>(([name, value]) => {
                                        if(value === undefined) {
                                            return null;
                                        }
                                        return [name, this.encode(value)];
                                    })
                                    .filter<[string, ESExpr]>(pair => pair !== null)
                            ),
                        };
                    }
            }
        },

        decode: function (expr: ESExpr): esexpr.DecodeResult<EStreeRepr> {
            switch(typeof expr) {
                case "string":
                case "number":
                case "bigint":
                case "boolean":
                    return { success: true, value: expr };

                case "object":
                    if(expr === null) {
                        return { success: true, value: null };
                    }
                    else if(ESExpr.isConstructor(expr)) {
                        switch(expr.name) {
                            case "list":
                                return esexpr.listCodec(this).decode(expr);

                            case "obj":
                                if(expr.args.length !== 0) {
                                    return {
                                        success: false,
                                        message: "Unexpected arguments for obj.",
                                        path: {
                                            type: "constructor",
                                            constructor: expr.name,
                                        },
                                    };
                                }

                                const o: PartialEstreeReprObject = { type: expr.name };
                                for(const [name, value] of expr.kwargs) {
                                    const propRes = this.decode(value);
                                    if(!propRes.success) {
                                        return {
                                            success: false,
                                            message: propRes.message,
                                            path: {
                                                type: "keyword",
                                                constructor: expr.name,
                                                keyword: name,
                                                next: propRes.path,
                                            },
                                        };
                                    }

                                    if(Object.prototype.hasOwnProperty(name)) {
                                        Object.defineProperty(o, name, { value: propRes.value });
                                    }
                                    else {
                                        o[name] = propRes.value;
                                    }
                                }
                                return { success: true, value: o };

                            case "RegExp":
                            {
                                if(expr.args.length !== 2) {
                                    return {
                                        success: false,
                                        message: "RegExp expects 2 positional arguments",
                                        path: {
                                            type: "constructor",
                                            constructor: expr.name,
                                        },
                                    };
                                }

                                if(expr.kwargs.size !== 0) {
                                    return {
                                        success: false,
                                        message: "RegExp expects no keyword arguments",
                                        path: {
                                            type: "constructor",
                                            constructor: expr.name,
                                        },
                                    };
                                }

                                const patternRes = esexpr.strCodec.decode(expr.args[0]!);
                                if(!patternRes.success) {
                                    return {
                                        success: false,
                                        message: patternRes.message,
                                        path: {
                                            type: "positional",
                                            constructor: expr.name,
                                            index: 0,
                                            next: patternRes.path,
                                        },
                                    };
                                }

                                const flagsRes = esexpr.strCodec.decode(expr.args[0]!);
                                if(!flagsRes.success) {
                                    return {
                                        success: false,
                                        message: flagsRes.message,
                                        path: {
                                            type: "positional",
                                            constructor: expr.name,
                                            index: 1,
                                            next: flagsRes.path,
                                        },
                                    };
                                }

                                return { success: true, value: new RegExp(patternRes.value, flagsRes.value) };
                            }


                            default:
                            {
                                if(expr.args.length !== 0) {
                                    return {
                                        success: false,
                                        message: "Unexpected arguments for estree value.",
                                        path: {
                                            type: "constructor",
                                            constructor: expr.name,
                                        },
                                    };
                                }

                                const o: PartialEstreeReprObject = { type: expr.name };
                                for(const [name, value] of expr.kwargs) {
                                    const propRes = this.decode(value);
                                    if(!propRes.success) {
                                        return {
                                            success: false,
                                            message: propRes.message,
                                            path: {
                                                type: "keyword",
                                                constructor: expr.name,
                                                keyword: name,
                                                next: propRes.path,
                                            },
                                        };
                                    }

                                    if(name === "type") {
                                        continue;
                                    }
                                    else if(Object.prototype.hasOwnProperty(name)) {
                                        Object.defineProperty(o, name, { value: propRes.value });
                                    }
                                    else {
                                        o[name] = propRes.value;
                                    }
                                }
                                return { success: true, value: o };
                            }
                        }
                    }
            }

            return {
                success: false,
                message: "Unexpected value for estree",
                path: {
                    type: "current",
                },
            };
        },
    };
}



export interface ImportHandler {
    getImportId(source: string): string;
}




export interface ImportedId {
    readonly localAlias: string;
    readonly source: string;
    readonly member?: string | undefined;
}

export namespace ImportedId {
    export const codec: ESExprCodec<ImportedId> = esexpr.lazyCodec(() =>
        esexpr.recordCodec<ImportedId>("imported-id", {
            localAlias: esexpr.keywordFieldCodec("local-alias", esexpr.strCodec),
            source: esexpr.keywordFieldCodec("source", esexpr.strCodec),
            member: esexpr.optionalKeywordFieldCodec("member", esexpr.undefinedOptionalCodec(esexpr.strCodec)),
        })
    );
}


type TraverseNodes = 
    | estree.Statement
    | estree.Expression
    | estree.Super
    | estree.SpreadElement
    | estree.PrivateIdentifier
    | estree.Pattern
    | estree.MethodDefinition
    | estree.PropertyDefinition
;



abstract class VariableScannerBase {

    scan(node: ReadonlyDeep<TraverseNodes>): void {
        this.traverse(node);
    }

    protected abstract withNestedScope(kind: "block" | "function", f: (scanner: VariableScannerBase) => void): void;
    protected abstract declare(name: string, kind: "let" | "const" | "var"): void;
    protected abstract identifierReference(name: string): void;


    protected traverse(node: ReadonlyDeep<TraverseNodes>): void {
        switch(node.type) {
            case "BlockStatement":
            case "StaticBlock":
                this.withNestedScope("block", scanner => {
                    for(const stmt of node.body) {
                        scanner.traverse(stmt);
                    }
                });
                break;

            case "VariableDeclaration":
                for(const decl of node.declarations) {
                    this.scanDeclaredVars(decl.id, node.kind);
                    this.traverse(decl.id);
                    if(decl.init) this.traverse(decl.init);
                }
                break;

            case "FunctionDeclaration":
            case "FunctionExpression":
            case "ArrowFunctionExpression":
                this.withNestedScope("function", scanner => {
                    if(node.type === "FunctionDeclaration" || node.type === "FunctionExpression") {
                        if(node.id) {
                            scanner.scanDeclaredVars(node.id, "const");
                        }
                    }

                    for(const param of node.params) {
                        scanner.scanDeclaredVars(param, "let");
                    }

                    scanner.traverse(node.body);
                });
                break;

            case "MethodDefinition":
            case "PropertyDefinition":
                if(node.computed) this.traverse(node.key);
                if(node.value) this.traverse(node.value);
                break;

            case "EmptyStatement":
            case "DebuggerStatement":
            case "ContinueStatement":
            case "BreakStatement":
            case "Literal":
            case "MetaProperty":
            case "ThisExpression":
            case "Super":
            case "PrivateIdentifier":
                break;

            case "Identifier":
                this.identifierReference(node.name);
                break;


            case "ExpressionStatement":
                this.traverse(node.expression);
                break;

            case "ReturnStatement":
            case "ThrowStatement":
                if(node.argument) this.traverse(node.argument);
                break;

            case "BinaryExpression":
            case "LogicalExpression":
                this.traverse(node.left);
                this.traverse(node.right);
                break;

            case "IfStatement":
                this.traverse(node.test);
                this.traverse(node.consequent);
                if(node.alternate) this.traverse(node.alternate);
                break;

            case "WhileStatement":
            case "DoWhileStatement":
                this.traverse(node.test);
                this.traverse(node.body);
                break;

            case "WithStatement":
                this.traverse(node.object);
                break;

            case "ForStatement":
                if(node.init) this.traverse(node.init);
                if(node.test) this.traverse(node.test);
                if(node.update) this.traverse(node.update);
                this.traverse(node.body);
                break;

            case "ForInStatement":
            case "ForOfStatement":
                this.withNestedScope("block", scanner => {
                    scanner.traverse(node.left);
                    scanner.traverse(node.right);
                    scanner.traverse(node.body);
                });
                break;


            case "LabeledStatement":
                this.traverse(node.body);
                break;

            case "SwitchStatement":
                this.traverse(node.discriminant);
                this.withNestedScope("block", scanner => {
                    for(const switchCase of node.cases) {
                        for(const stmt of switchCase.consequent) {
                            scanner.traverse(stmt);
                        }
                    }
                });
                break;

            case "TryStatement":
                this.traverse(node.block);
                this.withNestedScope("block", scanner => {
                    if(node.handler) {
                        if(node.handler.param) scanner.scanDeclaredVars(node.handler.param, "let");
                        scanner.traverse(node.handler.body);
                    }
                });
                if(node.finalizer) this.traverse(node.finalizer);
                break;

            case "ClassDeclaration":
            case "ClassExpression":
                if(node.superClass) this.traverse(node.superClass);
                this.withNestedScope("function", scanner => {
                    for(const stmt of node.body.body) {
                        scanner.traverse(stmt);
                    }
                });
                break;

            case "ArrayExpression":
            case "ArrayPattern":
                for(const element of node.elements) {
                    if(element === null) {
                        continue;
                    }
                    
                    this.traverse(node);
                }
                break;

            case "AssignmentExpression":
            case "AssignmentPattern":
                this.traverse(node.left);
                this.traverse(node.right);
                break;

            case "CallExpression":
            case "NewExpression":
                this.traverse(node.callee);
                for(const arg of node.arguments) {
                    this.traverse(arg);
                }
                break;

            case "MemberExpression":
                this.traverse(node.object);
                if (node.computed) {
                    this.traverse(node.property);
                }
                break;

            case "AwaitExpression":
            case "UnaryExpression":
            case "UpdateExpression":
            case "YieldExpression":
            case "SpreadElement":
            case "RestElement":
                if(node.argument) this.traverse(node.argument);
                break;

            case "ChainExpression":
                this.traverse(node.expression);
                break;

            case "ConditionalExpression":
                this.traverse(node.test);
                this.traverse(node.consequent);
                this.traverse(node.alternate);
                break;

            case "ImportExpression":
                this.traverse(node.source);
                break;

            case "ObjectExpression":
            case "ObjectPattern":
                for(const prop of node.properties) {
                    switch(prop.type) {
                        case "Property":
                            if(prop.computed) this.traverse(prop.key);
                            this.traverse(prop.value);
                            break;

                        case "SpreadElement":
                        case "RestElement":
                            this.traverse(prop.argument);
                            break;

                        default:
                            ensureExhaustive(prop);
                    }
                }
                break;

            case "SequenceExpression":
                for(const expr of node.expressions) {
                    this.traverse(expr);
                }
                break;

            case "TaggedTemplateExpression":
                this.traverse(node.tag);
                this.traverse(node.quasi);
                break;

            case "TemplateLiteral":
                for(const expr of node.expressions) {
                    this.traverse(expr);
                }
                break;

            default:
                ensureExhaustive(node);
        }
    }

    protected scanDeclaredVars(node: ReadonlyDeep<estree.Pattern>, kind: "let" | "const" | "var"): void {
        if(node === null) {
            return;
        }

        switch(node.type) {
            case "Identifier":
                this.declare(node.name, kind);
                break;

            case "ArrayPattern":
                for(const element of node.elements) {
                    if(element === null) {
                        continue;
                    }

                    this.scanDeclaredVars(element, kind);
                }
                break;

            case "AssignmentPattern":
                this.scanDeclaredVars(node.left, kind);
                break;

            case "ObjectPattern":
                for(const prop of node.properties) {
                    switch(prop.type) {
                        case "Property":
                            this.scanDeclaredVars(prop.value, kind);
                            break;

                        case "RestElement":
                            this.scanDeclaredVars(prop.argument, kind);
                            break;

                        default:
                            ensureExhaustive(prop);
                    }
                }
                break;

            case "RestElement":
                this.scanDeclaredVars(node.argument, kind);
                break;

            case "MemberExpression":
                break;

            default:
                ensureExhaustive(node);
        }
    }

}

class DeclaredVariableScanner extends VariableScannerBase {
    constructor(
        private readonly scope: Set<string>,
    ) {
        super();
    }

    override withNestedScope(kind: "block" | "function", f: (scanner: VariableScannerBase) => void): void {
        if(kind === "function") {
            return;
        }

        f(new DeclaredFunctionVarScanner(this.scope));
    }
    override declare(name: string, _kind: "let" | "const" | "var"): void {
        this.scope.add(name);
    }

    override identifierReference(_name: string): void {
    }

}

// Only finds "var", not "let" or "const". Used for nested scopes to find hoisted vars.
class DeclaredFunctionVarScanner extends DeclaredVariableScanner {
    override declare(name: string, kind: "let" | "const" | "var"): void {
        if(kind === "var") {
            super.declare(name, kind);
        }
    }
}

// Only finds "let" or "const", not "var". Used for nested scopes to exclude hoisted vars.
class DeclaredFunctionLetScanner extends DeclaredVariableScanner {
    override declare(name: string, kind: "let" | "const" | "var"): void {
        if(kind !== "var") {
            super.declare(name, kind);
        }
    }
}



class FreeVariableScanner extends VariableScannerBase {
    constructor(
        private readonly declaredVars: Set<string>,
        private readonly freeVars: Set<string>,
    ) {
        super();
    }

    override withNestedScope(kind: "block" | "function", f: (scanner: VariableScannerBase) => void): void {
        const nestedScope = new Set(this.declaredVars);

        let declScanner: DeclaredVariableScanner;
        if(kind === "function") {
            declScanner = new DeclaredVariableScanner(nestedScope);
        }
        else {
            declScanner = new DeclaredFunctionLetScanner(nestedScope);
        }

        f(declScanner);
        f(new FreeVariableScanner(nestedScope, this.freeVars));
    }
    
    override declare(_name: string, _kind: "let" | "const" | "var"): void {}

    override identifierReference(name: string): void {
        if(!this.declaredVars.has(name)) {
            this.freeVars.add(name);
        }
    }

    override scan(node: ReadonlyDeep<TraverseNodes>): void {
        new DeclaredVariableScanner(this.declaredVars).scan(node);
        super.scan(node);
    }

    // Nop since we ignore declarations.
    override scanDeclaredVars(_node: estree.Pattern, _kind: "let" | "const" | "var"): void {}
}

