import type * as tube from "@argon-lang/plugin-api/tube";
import { EmitModuleCommon } from "./emit_module_common.js";
import type * as estree from "estree";
import { getEscapedName, getOverloadExportName } from "./identifier.js";
import { argonRuntimeName } from "./module_emitter.js";


interface EmitState {
    readonly tailPosition: boolean;
    readonly functionResult: boolean;
    readonly discardValue: boolean;
    readonly scopeVars: estree.VariableDeclaration[];
}

namespace EmitState {
    export function subExpr(prev: EmitState): EmitState {
        return {
            tailPosition: false,
            functionResult: false,
            discardValue: false,
            scopeVars: prev.scopeVars,
        };
    };
    
    export async function emitScope(emitStatePartial: Omit<EmitState, "scopeVars">, f: (emitState: EmitState) => Promise<readonly estree.Statement[]>): Promise<estree.Statement[]> {
        const emitState = {
            ...emitStatePartial,
            scopeVars: [],
        };
        const result = await f(emitState);
        return [...emitState.scopeVars, ...result];
    }

    export function emitTopScope(f: (emitState: EmitState) => Promise<readonly estree.Statement[]>): Promise<estree.Statement[]> {
        return emitScope({
            tailPosition: true,
            functionResult: true,
            discardValue: false,
        }, f);
    }

    function statementBlockToExpression(stmts: estree.Statement[]): estree.Expression {
        if(stmts.length === 1 && stmts[0]?.type === "ReturnStatement" && stmts[0].argument !== undefined && stmts[0].argument !== null) {
            return stmts[0].argument;
        }
        else {
            return iefe(...stmts);
        }
    }

    export async function emitStandaloneScope(f: (emitState: EmitState) => Promise<estree.Expression>): Promise<estree.Expression> {
        const stmts = await emitScope({
            tailPosition: false,
            functionResult: false,
            discardValue: false,
        }, async emitState => [
            {
                type: "ReturnStatement",
                argument: await f(emitState),
            },
        ]);

        return statementBlockToExpression(stmts);
    }

    export function emitNestedScope(emitState: EmitState, f: (emitState: EmitState) => Promise<readonly estree.Statement[]>): Promise<estree.Statement[]> {
        return emitScope(emitState, f);
    }

    export async function emitNestedScopeExpr(emitState: EmitState, f: (emitState: EmitState) => Promise<estree.Expression>): Promise<estree.Expression> {
        const stmts = await emitNestedScope(emitState, async emitState => [
            {
                type: "ReturnStatement",
                argument: await f(emitState),
            }
        ]);

        return statementBlockToExpression(stmts);
    }

    
}



function id(name: string): estree.Identifier {
    return {
        type: "Identifier",
        name,
    };
}

function memberExpression(expr: estree.Expression | estree.Super, member: string | estree.Expression | estree.PrivateIdentifier): estree.MemberExpression {
    return {
        type: "MemberExpression",
        computed: false,
        optional: false,
        object: expr,
        property: typeof(member) === "string" ? id(member) : member,
    }
}

function callExpression(expr: estree.Expression | estree.Super, ...args: (estree.Expression | estree.SpreadElement)[]): estree.CallExpression {
    return {
        type: "CallExpression",
        optional: false,
        callee: expr,
        arguments: args,
    }
}

function literal(value: string | number | bigint | boolean | RegExp | null): estree.Literal {
    if(typeof(value) === "bigint") {
        return {
            type: "Literal",
            value,
            bigint: value.toString(),
        };
    }
    else if(value instanceof RegExp) {
        return {
            type: "Literal",
            value,
            regex: {
                pattern: value.source,
                flags: value.flags,
            },
        }
    }
    else {
        return {
            type: "Literal",
            value,
        };
    }
}


function declareVariable(kind: "const" | "let", name: string, value: estree.Expression): estree.VariableDeclaration {
    return {
        type: "VariableDeclaration",
        kind,
        declarations: [
            {
                type: "VariableDeclarator",
                id: id(name),
                init: value,
            }
        ],
    };
}

function declareConst(name: string, value: estree.Expression): estree.VariableDeclaration {
    return declareVariable("const", name, value);
}


function declareLet(name: string, value: estree.Expression): estree.VariableDeclaration {
    return declareVariable("let", name, value);
}


function iefe(...stmts: estree.Statement[]): estree.Expression {
    return {
        type: "CallExpression",
        optional: false,
        callee: {
            type: "ArrowFunctionExpression",
            expression: false,
            params: [],
            body: {
                type: "BlockStatement",
                body: stmts,
            }
        },
        arguments: [],
    };
}



export class ExprEmitter extends EmitModuleCommon {
    #getLocalVariableName(id: bigint): string {
        return `local_${id}`;
    }
    
    #getParameterVariableName(owner: tube.ParameterVariableOwner, index: number | bigint): string {
        let ownerStr: string;
        let ownerId: bigint;
        switch(owner.$case) {
            case "ClassOwner":
                ownerStr = "class";
                ownerId = owner.classId;
                break;

            case "TraitOwner":
                ownerStr = "trait";
                ownerId = owner.traitId;
                break;

            case "FunctionOwner":
                ownerStr = "function";
                ownerId = owner.functionId;
                break;

            case "MethodOwner":
                ownerStr = "method";
                ownerId = owner.methodId;
                break;

            case "ClassConstructorOwner":
                ownerStr = "classctor";
                ownerId = owner.classConstructorId;
                break;
        }

        return `param_${index}_${ownerStr}_${ownerId}`;
    }

    #getSigParamVarNames(decl: tube.ModuleDefinition.ElementDeclaration): readonly string[] {
        let varOwner: tube.ParameterVariableOwner;
        switch(decl.type) {
            case "ClassDeclaration":
                varOwner = {
                    $case: "ClassOwner",
                    classId: decl.id,
                };
                break;

            case "TraitDeclaration":
                varOwner = {
                    $case: "TraitOwner",
                    traitId: decl.id,
                };
                break;

            case "FunctionDeclaration":
                varOwner = {
                    $case: "FunctionOwner",
                    functionId: decl.id,
                };
                break;
        }


        return decl.signature.parameterTypes.map((param, index) =>
            this.#getParameterVariableName(varOwner, index)
        );
    }

    #getSigParams(owner: tube.ParameterVariableOwner, sig: { parameters: readonly tube.Parameter[] }): estree.Pattern[] {
        return sig.parameters
            .map<estree.Pattern | null>((param, index) => param.flags?.erased ? null : id(this.#getParameterVariableName(owner, index)))
            .filter((pat: estree.Pattern | null): pat is estree.Pattern => pat !== null);
    }
    
    async classExport(name: tube.Identifier | null, element: tube.ModuleDefinition.ElementDeclaration, arClass: tube.ClassDefinition): Promise<estree.ExportNamedDeclaration> {
        const exportName = getOverloadExportName(name, element.signature);
        const sigParamNames = this.#getSigParamVarNames(element);

        const ctorStmts = await this.#createClassCtors(arClass.constructors);
        const methodStmts = await this.#createMethods("methods", arClass.methods);
        const staticMethodStmts = await this.#createMethods("staticMethods", arClass.staticMethods);
        const fieldStmts: readonly estree.Statement[] = arClass.fields.map(field => {
            if(field.name.$case === undefined) {
                throw new Error("Field must have a name");
            }

            return {
                type: "ExpressionStatement",
                expression: {
                    type: "AssignmentExpression",
                    operator: "=",
                    left: memberExpression(id("fields"), id(getEscapedName(field.name))),
                    right: callExpression(id("Symbol")),
                }
            };
        });

        let baseClassExpr: estree.Expression | null;
        if(arClass.signature?.baseClass !== null) {
            const baseClass = arClass.signature.baseClass;
            baseClassExpr = await EmitState.emitStandaloneScope(emitState => this.#emitExpr(emitState, baseClass));
        }
        else {
            baseClassExpr = null;
        }

        const vtableStmts = await this.#emitVTable(element.id);

        return {
            type: "ExportNamedDeclaration",
            declaration: declareConst(
                exportName, 
                callExpression(
                    memberExpression(id(argonRuntimeName), id("createClass")),
                    {
                        type: "ArrowFunctionExpression",
                        expression: false,
                        params: sigParamNames.map(id),
                        body: {
                            type: "BlockStatement",
                            body: [
                                declareConst(
                                    "constructors",
                                    callExpression(memberExpression(id("Object"), "create"), literal(null)),
                                ),
                                ...ctorStmts,

                                declareConst(
                                    "methods",
                                    callExpression(memberExpression(id("Object"), "create"), literal(null)),
                                ),
                                ...methodStmts,

                                declareConst(
                                    "staticMethods",
                                    callExpression(memberExpression(id("Object"), "create"), literal(null)),
                                ),
                                ...staticMethodStmts,

                                declareConst(
                                    "fields",
                                    callExpression(memberExpression(id("Object"), "create"), literal(null)),
                                ),
                                ...fieldStmts,

                                declareLet("prototype", literal(null)),
                                
                                {
                                    type: "ReturnStatement",
                                    argument: {
                                        type: "ObjectExpression",
                                        properties: [
                                            {
                                                type: "Property",
                                                kind: "get",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("prototype"),
                                                value: {
                                                    type: "FunctionExpression",
                                                    params: [],
                                                    body: {
                                                        type: "BlockStatement",
                                                        body: [
                                                            {
                                                                type: "IfStatement",
                                                                test: {
                                                                    type: "BinaryExpression",
                                                                    operator: "===",
                                                                    left: id("prototype"),
                                                                    right: literal(null),
                                                                },
                                                                consequent: {
                                                                    type: "BlockStatement",
                                                                    body: [
                                                                        {
                                                                            type: "ExpressionStatement",
                                                                            expression: {
                                                                                type: "AssignmentExpression",
                                                                                operator: "=",
                                                                                left: id("prototype"),
                                                                                right: callExpression(
                                                                                    memberExpression(id("Object"), "create"),
                                                                                    baseClassExpr === null ? literal(null) : memberExpression(baseClassExpr, "prototype"),
                                                                                ),
                                                                            },
                                                                        },
                                                                        ...vtableStmts,
                                                                    ],
                                                                },
                                                            },
                                                        ],
                                                    },
                                                },
                                            },
                                            {
                                                type: "Property",
                                                kind: "init",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("constructors"),
                                                value: id("constructors"),
                                            },
                                            {
                                                type: "Property",
                                                kind: "init",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("methods"),
                                                value: id("methods"),
                                            },
                                            {
                                                type: "Property",
                                                kind: "init",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("staticMethods"),
                                                value: id("staticMethods"),
                                            },
                                            {
                                                type: "Property",
                                                kind: "init",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("fields"),
                                                value: id("fields"),
                                            },
                                        ],
                                    },
                                },
                            ],
                        },
                    },
                ),
            ),
            specifiers: [],
        };
    }
    
    async emitTrait(name: tube.Identifier | null, element: tube.ModuleDefinition.ElementDeclaration, arTrait: tube.TraitDefinition): Promise<estree.ExportNamedDeclaration> {
        const exportName = getOverloadExportName(name, element.signature);
        const sigParamNames = this.#getSigParamVarNames(element);

        const methodStmts = await this.#createMethods("methods", arTrait.methods);
        const staticMethodStmts = await this.#createMethods("staticMethods", arTrait.staticMethods);

        return {
            type: "ExportNamedDeclaration",
            declaration: declareConst(
                exportName, 
                callExpression(
                    memberExpression(id(argonRuntimeName), id("createClass")),
                    {
                        type: "ArrowFunctionExpression",
                        expression: false,
                        params: sigParamNames.map(id),
                        body: {
                            type: "BlockStatement",
                            body: [
                                declareConst(
                                    "methods",
                                    callExpression(memberExpression(id("Object"), "create"), literal(null)),
                                ),
                                ...methodStmts,

                                declareConst(
                                    "staticMethods",
                                    callExpression(memberExpression(id("Object"), "create"), literal(null)),
                                ),
                                ...staticMethodStmts,

                                declareLet("prototype", literal(null)),
                                
                                {
                                    type: "ReturnStatement",
                                    argument: {
                                        type: "ObjectExpression",
                                        properties: [
                                            {
                                                type: "Property",
                                                kind: "init",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("symbol"),
                                                value: callExpression(id("Symbol")),
                                            },
                                            {
                                                type: "Property",
                                                kind: "init",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("methods"),
                                                value: id("methods"),
                                            },
                                            {
                                                type: "Property",
                                                kind: "init",
                                                method: false,
                                                shorthand: false,
                                                computed: false,
                                                key: id("staticMethods"),
                                                value: id("staticMethods"),
                                            },
                                        ],
                                    },
                                },
                            ],
                        },
                    },
                ),
            ),
            specifiers: [],
        };
    }

    async emitFunction(name: tube.Identifier | null, element: tube.ModuleDefinition.ElementDeclaration, func: tube.FunctionDefinition): Promise<estree.ExportNamedDeclaration | null> {
        if(func?.flags?.erased) {
            return null;
        }

        const funcName = getOverloadExportName(name, element.signature);


        let decl: estree.FunctionDeclaration;
        switch(func.body?.$case) {
            case undefined:
                throw new Error("Missing function body");

            case "ExpressionBody":
            {
                const bodyExpr = func.body.expr;
                const params = this.#getSigParams({ $case: "FunctionOwner", functionId: element.id }, func.signature);
                const body = await EmitState.emitTopScope(emitState => this.#emitExprAsStmt(emitState, bodyExpr));
                decl = {
                    type: "FunctionDeclaration",
                    id: id(funcName),
                    async: true,
                    params,
                    body: {
                        type: "BlockStatement",
                        body,
                    },
                }
                break;
            }

            case "ExternalImplementation":
            {
                decl = await this.emitTubeInfo.getExternFunctionImplementation(func.body.id);
                break;
            }
        }

        return {
            type: "ExportNamedDeclaration",
            declaration: decl,
            specifiers: [],
        };
    }

    #createClassCtors(constructors: readonly tube.ClassConstructorMember[]): Promise<readonly estree.Statement[]> {
        return Promise.all(constructors.map(async member => {
            const ctor = await this.tube.getClassConstructor(member.id);
            const ctorExpr = await this.#createClassCtor(member, ctor);
            return {
                type: "ExpressionStatement",
                expression: {
                    type: "AssignmentExpression",
                    operator: "=",
                    left: memberExpression(id("constructors"), id(getOverloadExportName(null, member.signature))),
                    right: ctorExpr,
                },
            }
        }));
    }

    async #createClassCtor(member: tube.ClassConstructorMember, ctor: tube.ClassConstructorDefinition): Promise<estree.Expression> {
        switch(ctor.body?.$case) {
            case undefined:
                throw new Error("Missing class constructor body");

            case "ExpressionBody":
            {
                const impl = ctor.body;
                const params = this.#getSigParams({ $case: "ClassConstructorOwner", classConstructorId: member.id }, ctor.signature);

                const body = await EmitState.emitScope(
                    {
                        tailPosition: false,
                        functionResult: false,
                        discardValue: true,
                    },
                    async emitState => {
                        const preInit: readonly estree.Statement[] = (await Promise.all(impl.preInitialization.map(async (preInitStmt): Promise<readonly estree.Statement[]> => {
                            switch(preInitStmt.$case) {
                                case "OfExpr":
                                    return await this.#emitExprAsStmt(emitState, preInitStmt.expr);
        
                                case "FieldInitialization":
                                {
                                    const value = await this.#emitExpr(EmitState.subExpr(emitState), preInitStmt.value);
                                    const initExpr: estree.Expression = {
                                        type: "AssignmentExpression",
                                        operator: "=",
                                        left: {
                                            type: "MemberExpression",
                                            object: {
                                                type: "ThisExpression",
                                            },
                                            property: memberExpression(id("fields"), id(getEscapedName(preInitStmt.field))),
                                            computed: true,
                                            optional: false,
                                        },
                                        right: value,
                                    };
                                    return [
                                        {
                                            type: "ExpressionStatement",
                                            expression: initExpr,
                                        }
                                    ];
                                }
                            }
                        }))).flat(1);
        
                        
                        let baseCallStmt: readonly estree.Statement[];
                        if(impl.baseConstructorCall !== null) {
        
                            let ctorsExpr: estree.Expression;
                            if(member.id === impl.baseConstructorCall.id) {
                                ctorsExpr = id("constructors");
                            }
                            else {
                                ctorsExpr = memberExpression(
                                    await this.#emitExpr(EmitState.subExpr(emitState), impl.baseConstructorCall.classType),
                                    id("constructors"),
                                );
                            }
                            
                            const argExprs = await this.#emitArgExprs(EmitState.subExpr(emitState), impl.baseConstructorCall.args);
                            
                            baseCallStmt = [
                                {
                                    type: "ExpressionStatement",
                                    expression: callExpression(
                                        memberExpression(
                                            memberExpression(ctorsExpr, id(getOverloadExportName(null, member.signature))),
                                            id("call"),
                                        ),
                                        
                                        {
                                            type: "ThisExpression",
                                        },
                                        ...argExprs,
                                    ),
                                }
                            ];
        
                        }
                        else {
                            baseCallStmt = [];
                        }
        
                        const initThisStmt = [
                            declareConst(
                                this.#getLocalVariableName(impl.instanceVariable.id),
                                {
                                    type: "ThisExpression",
                                },
                            ),
                        ];
        
                        const postInit = await this.#emitExprAsStmt(emitState, impl.postInitialization);

                        return [
                            ...preInit,
                            ...baseCallStmt,
                            ...initThisStmt,
                            ...postInit,
                        ];
                    }
                );

                

                return {
                    type: "FunctionExpression",
                    params: [...params],
                    body: {
                        type: "BlockStatement",
                        body,
                    },
                };
            }

            case "ExternalImplementation":
            {
                const funcDecl = await this.emitTubeInfo.getExternClassConstructorImplementation(ctor.body.id);
                return {
                    type: "FunctionExpression",
                    id: funcDecl.id,
                    params: funcDecl.params,
                    generator: funcDecl.generator,
                    async: funcDecl.async,
                    body: funcDecl.body,
                };
            }
        }
    }

    async #createMethods(methodsVarName: string, methods: readonly tube.MethodMemberGroup[]): Promise<readonly estree.Statement[]> {
        return (await Promise.all(
            methods.flatMap(methodGroup =>
                methodGroup.methods.map(async (methodMember): Promise<estree.Statement | null> => {
                    const method = await this.tube.getMethod(methodMember.id);
                    const methodExpr = await this.#createMethod(methodMember, method);
                    if(methodExpr === null) {
                        return null;
                    }

                    const exportName = getOverloadExportName(methodGroup.name, methodMember.signature);
                    return {
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: memberExpression(id(methodsVarName), exportName),
                            right: {
                                type: "ObjectExpression",
                                properties: [
                                    {
                                        type: "Property",
                                        kind: "init",
                                        method: false,
                                        shorthand: false,
                                        computed: false,
                                        key: id("symbol"),
                                        value: callExpression(id("Symbol")),
                                    },
                                    {
                                        type: "Property",
                                        kind: "init",
                                        method: false,
                                        shorthand: false,
                                        computed: false,
                                        key: id("implementation"),
                                        value: methodExpr,
                                    },
                                ],
                            },
                        },
                    };
                })
            )
        ))
            .filter<estree.Statement>((stmt): stmt is estree.Statement => stmt !== null);
    }

    async #createMethod(methodMember: tube.MethodMember, method: tube.MethodDefinition): Promise<estree.Expression | null> {
        if(method?.flags?.erased) {
            return null;
        }

        switch(method.body?.$case) {
            case undefined:
                throw new Error("Missing function body");

            case "ExpressionBody":
            {
                const bodyExpr = method.body.expr;
                const params = this.#getSigParams({ $case: "FunctionOwner", functionId: methodMember.id }, method.signature);
                const body = await EmitState.emitTopScope(emitState => this.#emitExprAsStmt(emitState, bodyExpr));
                return {
                    type: "FunctionExpression",
                    params,
                    async: true,
                    body: {
                        type: "BlockStatement",
                        body,
                    },
                };
            }

            case "ExternalImplementation":
            {
                const funcDecl = await this.emitTubeInfo.getExternMethodImplementation(method.body.id);
                return {
                    type: "FunctionExpression",
                    id: funcDecl.id,
                    params: funcDecl.params,
                    generator: funcDecl.generator,
                    async: funcDecl.async,
                    body: funcDecl.body,
                }
            }
        }
    }


    async #emitExprAsStmt(emitState: EmitState, expr: tube.Expr): Promise<estree.Statement[]> {
        switch(expr.$case) {
            case "BindVariable":
                if(expr.definition.flags.erased) {
                    if(emitState.discardValue) {
                        return [];
                    }
                    else {
                        return [
                            {
                                type: "ReturnStatement",
                                argument: {
                                    type: "ArrayExpression",
                                    elements: [],
                                },
                            },
                        ];
                    }
                }
                else {
                    const valueExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.value);
                    const varName = this.#getLocalVariableName(expr.definition.id);
                    return [
                        declareVariable(
                            expr.definition.flags.mutable ? "let" : "const",
                            varName,
                            valueExpr,
                        ),
                    ];
                }
            
            case "Sequence":
            {
                return await EmitState.emitNestedScope(emitState, async emitState => {
                    const subExprs: estree.Statement[] = [];
                    for(const [i, subExpr] of expr.exprs.entries()) {
                        const subEmitState: EmitState = {
                            ...emitState,
                            tailPosition: emitState.tailPosition && i === expr.exprs.length - 1,
                            functionResult: emitState.functionResult && i === expr.exprs.length - 1,
                            discardValue: emitState.discardValue || i !== expr.exprs.length - 1,
                        }
                        subExprs.push(...await this.#emitExprAsStmt(subEmitState, subExpr));
                    }
                    return subExprs;    
                });
            }

            case "IfElse":
            {
                this.ensureRawImportName({ tube: { name: ["Argon", "Core"] }, path: { path: [ "Bool" ] } }, "boolValueSymbol");
                const { whenTrue, whenFalse } = expr;

                const condExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.condition);
                const trueBody = await EmitState.emitNestedScope(emitState, emitState => this.#emitExprAsStmt(emitState, whenTrue));
                const falseBody = await EmitState.emitNestedScope(emitState, emitState => this.#emitExprAsStmt(emitState, whenFalse));
                return [
                    {
                        type: "IfStatement",
                        test: condExpr,
                        consequent: {
                            type: "BlockStatement",
                            body: trueBody,
                        },
                        alternate: {
                            type: "BlockStatement",
                            body: falseBody,
                        },
                    },
                ];
            }

            case "EnsureExecuted":
            {
                const bodyStmts = await this.#emitExprAsStmt({ ...emitState, tailPosition: false }, expr.body);
                const ensuringBodyStmts = await this.#emitExprAsStmt(EmitState.subExpr(emitState), expr.ensuring);
                return [
                    {
                        type: "TryStatement",
                        block: {
                            type: "BlockStatement",
                            body: bodyStmts,
                        },
                        finalizer: {
                            type: "BlockStatement",
                            body: ensuringBodyStmts,
                        },
                    },
                ];
            }

            case "Proving":
                return this.#emitExprAsStmt(emitState, expr.inner);

            default:
                if(emitState.discardValue) {
                    if(expr.$case === "LoadTuple" && expr.values.length === 0) {
                        return [];
                    }
                    else {
                        return [
                            {
                                type: "ExpressionStatement",
                                expression: await this.#emitExpr(emitState, expr),
                            },
                        ];
                    }
                }
                else {
                    return [
                        {
                            type: "ReturnStatement",
                            argument: await this.#emitExpr(emitState, expr),
                        },
                    ];
                }
        }
    }

    async #emitExpr(emitState: EmitState, expr: tube.Expr): Promise<estree.Expression> {
        if(emitState.tailPosition) {
            let callExpr: estree.Expression;
            switch(expr.$case) {
                case "FunctionCall":
                    callExpr = await this.#emitFunctionCall(emitState, expr);
                    break;

                case "FunctionObjectCall":
                {
                    const funcExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.funcObject);
                    const argExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.arg);
                    callExpr = callExpression(funcExpr, argExpr)
                    break;
                }
                
                case "MethodCall":
                    callExpr = await this.#emitMethodCall(emitState, expr);
                    break;

                default:
                {
                    return this.#emitExpr({ ...emitState, tailPosition: false }, expr);
                }
            }

            return callExpression(
                memberExpression(memberExpression(id(argonRuntimeName), "trampoline"), "delay"),
                {
                    type: "ArrowFunctionExpression",
                    async: true,
                    expression: true,
                    params: [],
                    body: callExpr,
                },
            );
        }
        else if(emitState.functionResult) {
            const convExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr);
            return callExpression(
                memberExpression(memberExpression(id(argonRuntimeName), "trampoline"), "result"),
                convExpr,
            );
        }
        else {
            switch(expr.$case) {
                case "BindVariable":
                    if(expr.definition.flags.erased) {
                        return {
                            type: "ArrayExpression",
                            elements: [],
                        };
                    }
                    else {
                        const valueExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.value);
                        const varName = this.#getLocalVariableName(expr.definition.id);
                        const decl: estree.VariableDeclaration = {
                            type: "VariableDeclaration",
                            kind: "let",
                            declarations: [
                                {
                                    type: "VariableDeclarator",
                                    id: id(varName),
                                },
                            ],
                        };
                        emitState.scopeVars.push(decl);

                        const convExpr: estree.Expression = {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: id(varName),
                            right: valueExpr,
                        };

                        if(emitState.discardValue) {
                            return convExpr;
                        }
                        else {
                            return iefe(
                                {
                                    type: "ExpressionStatement",
                                    expression: convExpr,
                                },
                                {
                                    type: "ReturnStatement",
                                    argument: {
                                        type: "ArrayExpression",
                                        elements: [],
                                    },
                                },
                            );
                        }
                    }

                case "ClassConstructorCall":
                {
                    const signature = await this.getClassConstructorErasedSignature(expr.id, expr.classType);
                    const keyName = getOverloadExportName(null, signature);
                    
                    const instanceTypeExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.classType);
                    const argExprs = await this.#emitArgExprs(emitState, expr.args);

                    return callExpression(
                        memberExpression(id(argonRuntimeName), "createObject"),
                        memberExpression(instanceTypeExpr, "prototype"),
                        memberExpression(memberExpression(instanceTypeExpr, "constructors"), keyName),
                        ...argExprs,
                    );
                }

                case "FunctionCall":
                {
                    const callExpr = await this.#emitFunctionCall(emitState, expr);
                    return memberExpression(
                        {
                            type: "AwaitExpression",
                            argument: callExpression(
                                memberExpression(
                                    memberExpression(
                                        id(argonRuntimeName),
                                        "trampoline",
                                    ),
                                    "resolve",
                                ),
                                callExpr,
                            ),
                        },
                        "value",
                    );
                }

                case "IfElse":
                {
                    this.ensureRawImportName({ tube: { name: [ "Argon", "Core" ] }, path: { path: [ "Bool" ] } }, "boolValueSymbol");

                    const { whenTrue, whenFalse } = expr;

                    const condExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.condition);
                    const trueBodyExpr = await EmitState.emitNestedScopeExpr(emitState, emitState => this.#emitExpr(emitState, whenTrue));
                    const falseBodyExpr = await EmitState.emitNestedScopeExpr(emitState, emitState => this.#emitExpr(emitState, whenFalse));

                    return {
                        type: "ConditionalExpression",
                        test: {
                            type: "MemberExpression",
                            computed: true,
                            optional: false,
                            object: condExpr,
                            property: id("boolValueSymbol"),
                        },
                        consequent: trueBodyExpr,
                        alternate: falseBodyExpr,
                    };
                }

                case "MethodCall":
                {
                    const callExpr = await this.#emitMethodCall(emitState, expr);
                    return memberExpression(
                        {
                            type: "AwaitExpression",
                            argument: callExpression(
                                memberExpression(
                                    memberExpression(
                                        id(argonRuntimeName),
                                        "trampoline",
                                    ),
                                    "resolve",
                                ),
                                callExpr,
                            ),
                        },
                        "value",
                    );
                }

                case "LoadConstantBool":
                    this.ensureRawImportName({ tube: { name: [ "Argon", "Core" ] }, path: { path: [ "Bool" ] } }, "createBool");
                    return callExpression(id("createBool"), literal(expr.b));

                case "LoadConstantInt":
                    this.ensureRawImportName({ tube: { name: [ "Argon", "Core" ] }, path: { path: [ "Int" ] } }, "createInt");
                    return callExpression(id("createInt"), literal(expr.i));

                case "LoadConstantString":
                    this.ensureRawImportName({ tube: { name: [ "Argon", "Core" ] }, path: { path: [ "String" ] } }, "createString");
                    return callExpression(id("createString"), literal(expr.s));

                case "LoadLambda":
                {
                    const { body } = expr;
                    const varName = this.#getLocalVariableName(expr.argVariable.id);
                    const bodyExpr = await EmitState.emitNestedScope(emitState, emitState =>
                        this.#emitExprAsStmt({ ...emitState, tailPosition: true, discardValue: false }, body)
                    );

                    return {
                        type: "ArrowFunctionExpression",
                        expression: false,
                        params: [ id(varName) ],
                        body: {
                            type: "BlockStatement",
                            body: bodyExpr,
                        },
                    };
                }

                case "LoadTuple":
                {
                    const argExprs: estree.Expression[] = [];
                    for(const arg of expr.values) {
                        argExprs.push(await this.#emitExpr(EmitState.subExpr(emitState), arg));
                    }

                    return {
                        type: "ArrayExpression",
                        elements: argExprs,
                    };
                }

                case "LoadTupleElement":
                {
                    const tupleExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.tuple);
                    return {
                        type: "MemberExpression",
                        computed: true,
                        optional: false,
                        object: tupleExpr,
                        property: literal(Number(expr.index)),
                    }
                }

                case "LoadVariable":
                    return this.#getVariableExpr(expr.varRef);

                case "StoreVariable":
                {
                    const varExpr = this.#getVariableExpr(expr.varRef);
                    if(varExpr.type === "ThisExpression") {
                        throw new Error("Cannot reassign instance variable");
                    }

                    const valueExpr = await this.#emitExpr(EmitState.subExpr(emitState), expr.value);
                    return {
                        type: "AssignmentExpression",
                        operator: "=",
                        left: varExpr,
                        right: valueExpr,
                    };
                }

                case "ClassType":
                {
                    const specifier = await this.getClassImportSpecifier(expr.id);
                    const importName = this.getImportName(specifier);
                    const args = await this.#emitArgExprs(emitState, expr.args);
                    return callExpression(id(importName), ...args);
                }

                case "TraitType":
                {
                    const specifier = await this.getTraitImportSpecifier(expr.id);
                    const importName = this.getImportName(specifier);
                    const args = await this.#emitArgExprs(emitState, expr.args);
                    return callExpression(id(importName), ...args);
                }

                case "Proving":
                    return this.#emitExpr(emitState, expr.inner);


                case "EnsureExecuted":
                {
                    const stmts = await this.#emitExprAsStmt(emitState, expr);
                    return iefe(...stmts);
                }

                default:
                    throw new Error(`Unimplemented expression type: ${expr.$case}`)
            }
        }
    }

    async #emitFunctionCall(emitState: EmitState, expr: tube.Expr.FunctionCall): Promise<estree.Expression> {
        const specifier = await this.getFunctionImportSpecifier(expr.id);
        const importName = this.getImportName(specifier);
        const args = await this.#emitArgExprs(emitState, expr.args);
        return callExpression(id(importName), ...args);
    }

    async #emitMethodCall(emitState: EmitState, expr: tube.Expr.MethodCall): Promise<estree.Expression> {
        const { isStatic, name, signature } = await this.getMethodInfo(expr.methodId, expr.ownerType);

        const instance = await this.#emitExpr(EmitState.subExpr(emitState), expr.instance);
        const ownerType = await this.#emitExpr(EmitState.subExpr(emitState), expr.ownerType);
        const methodKeyName = getOverloadExportName(name, signature);
        
        const args = await this.#emitArgExprs(emitState, expr.args);
        
        if(isStatic) {
            return callExpression(
                memberExpression(
                    memberExpression(
                        memberExpression(
                            memberExpression(ownerType, "staticMethods"),
                            methodKeyName
                        ),
                        "implementation",
                    ),
                    "call",    
                ),
                instance,
                ...args,
            );
        }
        else {
            return callExpression(
                memberExpression(
                    memberExpression(
                        {
                            type: "MemberExpression",
                            computed: true,
                            optional: false,
                            object: instance,
                            property: memberExpression(ownerType, "methods")
                        },
                        methodKeyName
                    ),
                    "symbol",
                ),
                instance,
                ...args,
            );
        }
    }

    async #emitArgExprs(emitState: EmitState, args: readonly tube.Argument[]): Promise<readonly estree.Expression[]> {
        const argExprs: estree.Expression[] = [];
        for(const arg of args) {
            if(arg.flags.erased) {
                continue;
            }

            argExprs.push(await this.#emitExpr(EmitState.subExpr(emitState), arg.expr));
        }
        return argExprs;
    }
    

    async #emitVTable(classId: bigint): Promise<readonly estree.Statement[]> {
        const vtable = await this.emitTubeInfo.getVTableDiff(classId);
        const stmts: estree.Statement[] = [];

        for(const entry of vtable.entries) {
            const slotMethodId = entry.methodId;
            const { signature: slotSig } = await this.getMethodInfo(slotMethodId, entry.owner);
            const { signature: implSig } = await this.getMethodInfo(entry.implMethod, entry.implOwner);
            const slotMethodsExpr = await this.#getMethodsExprForInstanceType(classId, entry.owner);
            const implMethodsExpr = await this.#getMethodsExprForInstanceType(classId, entry.implOwner);

            const slotSymbol = memberExpression(memberExpression(slotMethodsExpr, getOverloadExportName(entry.name, slotSig)), "symbol");
            const implMethod = memberExpression(memberExpression(implMethodsExpr, getOverloadExportName(entry.name, implSig)), "implementation");

            stmts.push({
                type: "ExpressionStatement",
                expression: {
                    type: "AssignmentExpression",
                    operator: "=",
                    left: {
                        type: "MemberExpression",
                        computed: true,
                        optional: false,
                        object: id("prototype"),
                        property: slotSymbol,
                    },
                    right: implMethod,
                },
            });
        }
        
        return stmts;
    }

    async #getMethodsExprForInstanceType(ownerClassId: bigint, instanceType: tube.MethodOwnerType): Promise<estree.Expression> {
        if(instanceType.$case === "ClassType" && instanceType.id === ownerClassId) {
            return id("methods");
        }

        return memberExpression(
            await EmitState.emitStandaloneScope(emitState => this.#emitExpr(emitState, instanceType)),
            "methods",
        );
    }

    #getVariableExpr(variable: tube.VariableReference): estree.Identifier | estree.MemberExpression | estree.ThisExpression {
        switch(variable.$case) {
            case "LocalVariableReference":
                return id(this.#getLocalVariableName(variable.id));

            case "ParameterVariableReference":
                return id(this.#getParameterVariableName(variable.owner, variable.parameterIndex));

            case "InstanceVariableReference":
                return {
                    type: "ThisExpression",
                };

            case "MemberVariableReference":
                return memberExpression(
                    {
                        type: "MemberExpression",
                        computed: true,
                        optional: false,
                        object: {
                            type: "ThisExpression",
                        },
                        property: id("fields"),
                    },
                    getEscapedName(variable.name),
                )

            case "FunctionResultVariableReference":
                throw new Error("Unexpected function result variable");
        }
    }

}
