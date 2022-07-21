
export class Delay<T> {
    constructor(public readonly value: () => Trampoline<T>) {}
}

export class Result<T> {
    constructor(public readonly value: T) {}
}

export type Trampoline<T> =
    Delay<T> |
    Promise<Delay<T>> |
    Result<T>;

export async function jump<T>(tramp: Trampoline<T>): Promise<Result<T>> {
    tramp = await tramp;
    while(tramp instanceof Delay) {
        tramp = await tramp.value();
    }

    return tramp;
}

export function delay<T>(value: () => Trampoline<T>): Trampoline<T> {
    return new Delay<T>(value);
}

export function result<T>(value: T): Trampoline<T> {
    return new Result<T>(value);
}

