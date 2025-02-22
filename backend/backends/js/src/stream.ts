import type { ScopedResource, Stream } from "@argon-lang/js-backend-api";

export async function* streamToAsyncIterable<E, A>(streamFunc: () => Promise<ScopedResource<Stream<E, A>>>): AsyncIterable<A> {
    const streamRes = await streamFunc();
    try {
        const stream = await streamRes.get();
        while(true) {
            const entries = await stream.next();
            if(entries.length === 0) {
                break;
            }

            yield* entries;
        }
    }
    finally {
        await streamRes.close();
    }
}


export async function asyncIterableToStream<E, A>(iterable: AsyncIterable<A>): Promise<ScopedResource<Stream<E, A>>> {
    const iter = iterable[Symbol.asyncIterator]();

    let isDone = false;
    const stream: Stream<E, A> = {
        async next() {
            const res = await iter.next();
            if(res.done) {
                isDone = true;
                return [];
            }

            return [ res.value ];
        },
    };

    return {
        async get() {
            return stream;
        },

        async close() {
            if(!isDone && iter.return) {
                await iter.return();
            }
        },
    };
}
