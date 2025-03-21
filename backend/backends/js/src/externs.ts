import { ensureExhaustive } from "./util.js";

import type * as estree from "estree";
import type { ReadonlyDeep } from "type-fest";
import type { ExternFunction, ImportedId } from "./platform-data.js";

export interface ExternProvider {
    getExternFunction(name: string): ExternFunction | undefined;
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

    getExternFunction(name: string): ExternFunction | undefined {
        const extern = this.#externs.get(name);
        if(extern === undefined) {
            return undefined;
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

