import * as esexpr from "@argon-lang/esexpr";
import type { ReadonlyDeep } from "type-fest";
import type * as estree from "estree";
import { ESExpr, type ESExprCodec, type ESExprTag } from "@argon-lang/esexpr";
import { name as isValidName } from "estree-util-is-identifier-name";
import type { ImportHandler } from "./imports.js";


export interface JSTubeMetadata {
    // readonly packageName?: string | undefined;
    // readonly externalSubpath: string | undefined;
    // readonly internalSubpath: string;
}

export namespace JSTubeMetadata {
    export const codec: ESExprCodec<JSTubeMetadata> = esexpr.lazyCodec(() =>
        esexpr.recordCodec<JSTubeMetadata>("js-tube-metadata", {
        })
    );
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

        isEncodedEqual(a: EStreeRepr, b: EStreeRepr): boolean {
            if(
                typeof a === "string" ||
                typeof a === "number" ||
                typeof a === "bigint" ||
                typeof a === "boolean" ||
                a === null
            ) {
                return a === b;
            }
            else if(a instanceof RegExp) {
                return b instanceof RegExp &&
                    a.source === b.source &&
                    a.flags === b.flags;
            }
            else if(a instanceof Array) {
                return b instanceof Array &&
                    a.length === b.length &&
                    a.every((ai, i) => this.isEncodedEqual(ai, b[i]!));
            }
            else {
                if(typeof b !== "object" || b instanceof RegExp || b instanceof Array || b === null) {
                    return false;
                }

                const keysA = Object.keys(a);
                const keysB = Object.keys(b);
                if (keysA.length !== keysB.length) {
                    return false;
                }


                for(const key of keysA) {
                    if(!(key in b)) {
                        return false;
                    }

                    const ai = a[key];
                    const bi = b[key];

                    if(ai === undefined) {
                        if(bi !== undefined) {
                            return false;
                        }

                        continue;
                    }

                    if(bi === undefined) {
                        return false;
                    }

                    if(!this.isEncodedEqual(ai, bi)) {
                        return false;
                    }
                }

                return true;
            }
        },

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


