import type * as estree from "estree";
import {type DecodeResult, ESExpr, type ESExprCodec, type ESExprTagSet} from "@argon-lang/esexpr";
import * as esexpr from "@argon-lang/esexpr";
import type {FunctionDeclaration} from "estree";
import type {ReadonlyDeep} from "type-fest";


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

type PartialEstreeReprObject = { [k: string]: EStreeRepr | undefined };

const nodeCodec: ESExprCodec<ReadonlyDeep<estree.Node>> = {
    get tags(): ESExprTagSet { return "all-tags"; },

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
} satisfies ESExprCodec<EStreeRepr> as unknown as ESExprCodec<estree.Node>;


export type EstreeFunctionDeclaration = ReadonlyDeep<estree.FunctionDeclaration>;

export namespace EstreeFunctionDeclaration {
    export const codec: ESExprCodec<ReadonlyDeep<estree.FunctionDeclaration>> = {
        get tags(): ESExprTagSet {
            return new Set(["FunctionDeclaration"]);
        },

        isEncodedEqual(a: FunctionDeclaration, b: FunctionDeclaration): boolean {
            return nodeCodec.isEncodedEqual(a, b);
        },

        encode(value: FunctionDeclaration): ESExpr {
            return nodeCodec.encode(value);
        },

        decode(expr: ESExpr): DecodeResult<ReadonlyDeep<FunctionDeclaration>> {
            let result = nodeCodec.decode(expr);
            if(!result.success) {
                return result;
            }

            if(result.value.type !== "FunctionDeclaration") {
                return {
                    success: false,
                    message: "Expected FunctionDeclaration node, got " + result.value.type,
                    path: {
                        type: "current",
                    },
                };
            }

            return {
                success: true,
                value: result.value,
            };
        },
    };
}

