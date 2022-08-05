
export class Delay<T> {
    constructor(public readonly value: () => Promise<Trampoline<T>>) {}
}

export class Result<T> {
    constructor(public readonly value: T) {}
}

export type Trampoline<T> =
    Delay<T> |
    Result<T>;

export async function resolve<T>(trampPromise: Promise<Trampoline<T>>): Promise<Result<T>> {
    let tramp = await trampPromise;
    while(tramp instanceof Delay) {
        tramp = await tramp.value();
    }
1
    return tramp;
}

export function delay<T>(value: () => Promise<Trampoline<T>>): Trampoline<T> {
    return new Delay<T>(value);
}

export function result<T>(value: T): Trampoline<T> {
    return new Result<T>(value);
}

