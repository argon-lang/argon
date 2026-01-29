import { Mutex } from "async-mutex";

export class Monitor {
    readonly #mutex = new Mutex();
    #waiters: (() => void)[] = [];

    async runExclusive<T>(callback: () => T | Promise<T>): Promise<T> {
        return await this.#mutex.runExclusive(callback);
    }

    async wait(): Promise<void> {
        if(!this.#mutex.isLocked()) {
            throw new Error("Monitor.wait called without holding the lock");
        }

        const { promise: waitPromise, resolve: release } = Promise.withResolvers<void>();

        this.#waiters.push(release);

        this.#mutex.release();
        try {
            await waitPromise;
        }
        finally {
            await this.#mutex.acquire();
        }
    }

    notify(): void {
        if(!this.#mutex.isLocked()) {
            throw new Error("Monitor.notify called without holding the lock");
        }

        const waiter = this.#waiters.shift();
        if (waiter) {
            waiter();
        }
    }

    notifyAll(): void {
        if(!this.#mutex.isLocked()) {
            throw new Error("Monitor.notifyAll called without holding the lock");
        }

        const waiters = this.#waiters;
        this.#waiters = [];
        for (const waiter of waiters) {
            waiter();
        }
    }
}



