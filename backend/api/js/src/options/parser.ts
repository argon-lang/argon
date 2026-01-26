import { Option } from "@argon-lang/esexpr";
import type { BinaryResource, DirectoryResource } from "../index.js";
import { OptionParseFailure, type OptionValue, type OptionValueAtom } from "../options.js";



export type OptionsSpec<E, T> = {
    readonly [K in keyof T]: OptionSpec<E, T[K]>;
};

export interface OptionSpec<E, A> {
    defaultValue: Option<A>;
    fromValue(name: string, value: OptionValue<E>): A;
}

export interface OptionAtomSpec<E, A> {
    fromValue(name: string, value: OptionValueAtom<E>): A;
}


type OptionsBuilder<T> = {
    -readonly [K in keyof T]?: T[K];
};


export function parse<E, T>(options: ReadonlyMap<string, OptionValue<E>>, spec: OptionsSpec<E, T>): T {
    const o: OptionsBuilder<T> = {};
    const seenOptions = new Set<string>();

    for(const [k, value] of options) {
        if(seenOptions.has(k)) {
            throw OptionParseFailure.createError(
                {
                    name: k,
                    reason: {
                        $type: "duplicate-value",
                    },
                },
                `Option ${k} was specified more than once`
            );
        }

        seenOptions.add(k);

        if(!(k in spec)) {
            throw OptionParseFailure.createError(
                {
                    name: k,
                    reason: {
                        $type: "unknown-option",
                    },
                },
                `Unknown option ${k} was specified`
            );
        }

        const k2 = k as keyof T;
        o[k2] = spec[k2].fromValue(k, value);
    }

    for(const k of Object.keys(spec) as (keyof T & string)[]) {
        if(seenOptions.has(k)) {
            continue;
        }

        const defaultValue = spec[k].defaultValue;
        if(defaultValue !== null) {
            o[k] = Option.get(defaultValue);
            continue;
        }

        throw OptionParseFailure.createError(
            {
                name: k,
                reason: {
                    $type: "missing-value",
                },
            },
            `Option ${k} was not specified`
        );
    }

    return o as T;
}



export function optional<E, A>(atomSpec: OptionAtomSpec<E, A>): OptionSpec<E, A | undefined> {
    return {
        defaultValue: Option.some(undefined),

        fromValue(name: string, value: OptionValue<E>): A | undefined {
            if(value.$type !== "single-value") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "wrong-occurrence",
                        },
                    },
                    `Optional backend option ${name} was given many-values.`
                );
            }


            return atomSpec.fromValue(name, value.value);
        },
    };
}

export function required<E, A>(atomSpec: OptionAtomSpec<E, A>): OptionSpec<E, A> {
    return {
        defaultValue: null,

        fromValue(name: string, value: OptionValue<E>): A {
            if(value.$type !== "single-value") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "wrong-occurrence",
                        },
                    },
                    `Required backend option ${name} was given many-values.`
                );
            }


            return atomSpec.fromValue(name, value.value);
        },
    };
}

export function many<E, A>(atomSpec: OptionAtomSpec<E, A>): OptionSpec<E, readonly A[]> {
    return {
        defaultValue: Option.some([]),

        fromValue(name: string, value: OptionValue<E>): readonly A[] {
            if(value.$type !== "many-values") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "wrong-occurrence",
                        },
                    },
                    `Many backend option ${name} was given a single-value.`
                );
            }

            const items: A[] = [];
            items.push(atomSpec.fromValue(name, value.head));
            for(const a of value.tail) {
                items.push(atomSpec.fromValue(name, a));
            }


            return items;
        },
    };
}

export function manyRequired<E, A>(atomSpec: OptionAtomSpec<E, A>): OptionSpec<E, readonly [ A, ...A[] ]> {
    return {
        defaultValue: null,

        fromValue(name: string, value: OptionValue<E>): readonly [ A, ...A[] ] {
            if(value.$type !== "many-values") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "wrong-occurrence",
                        },
                    },
                    `Many required backend option ${name} was given a single-value.`
                );
            }

            const items: [ A, ...A[] ] = [
                atomSpec.fromValue(name, value.head),
            ];

            for(const a of value.tail) {
                items.push(atomSpec.fromValue(name, a));
            }

            return items;
        },
    };
}

export function stringOption<E>(): OptionAtomSpec<E, string> {
    return {
        fromValue(name: string, value: OptionValueAtom<E>): string {
            if(value.$type !== "string") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "unexpected-type",
                        }
                    },
                    `Expected a string, but got ${value.$type}`
                );
            }

            return value.s;
        },
    };
}

export function boolOption<E>(): OptionAtomSpec<E, boolean> {
    return {
        fromValue(name: string, value: OptionValueAtom<E>): boolean {
            if(value.$type !== "bool") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "unexpected-type",
                        }
                    },
                    `Expected a bool, but got ${value.$type}`
                );
            }

            return value.b;
        },
    };
}

export function binaryResourceOption<E>(): OptionAtomSpec<E, BinaryResource<E>> {
    return {
        fromValue(name: string, value: OptionValueAtom<E>): BinaryResource<E> {
            if(value.$type !== "binary-resource") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "unexpected-type",
                        }
                    },
                    `Expected a binary-resource, but got ${value.$type}`
                );
            }

            return value.res;
        },
    };
}

export function directoryResourceOption<E>(): OptionAtomSpec<E, DirectoryResource<E>> {
    return {
        fromValue(name: string, value: OptionValueAtom<E>): DirectoryResource<E> {
            if(value.$type !== "directory-resource") {
                throw OptionParseFailure.createError(
                    {
                        name,
                        reason: {
                            $type: "unexpected-type",
                        }
                    },
                    `Expected a directory-resource, but got ${value.$type}`
                );
            }

            return value.res;
        },
    };
}


