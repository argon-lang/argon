import type {BackendFactory, HostOperations, SimpleBackendFactory} from "@argon-lang/js-backend-api/factory.js";
import type {BackendMetadata} from "@argon-lang/js-backend-api/metadata.js";
import type {ErrorChecker} from "@argon-lang/noble-idl-core/util";
import type {Backend} from "@argon-lang/js-backend-api";

export class JSBackendFactory implements BackendFactory {
    constructor(
        public readonly metadata: BackendMetadata,
        simpleFactory: SimpleBackendFactory,
    ) {
        this.#simpleFactory = simpleFactory;
    }

    readonly #simpleFactory: SimpleBackendFactory;

    create<E, A>(errorChecker_e: ErrorChecker<E>, hostOperations: HostOperations<E>, f: <Output>(backend: Backend<E, Output>) => A): A {
        return this.#simpleFactory.create(errorChecker_e, hostOperations, f);
    }

    close(): Promise<void> {
        return Promise.resolve();
    }
}
