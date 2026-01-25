import type { ErrorChecker } from "@argon-lang/noble-idl-core/util";
import type {Backend} from "./index.js";
import type {BackendMetadata} from "./metadata.js";

export interface BackendFactory {
    readonly metadata: BackendMetadata;
    
    create<E, A>(
        errorChecker: ErrorChecker<E>,
        hostOperations: HostOperations<E>,
        f: <Output>(backend: Backend<E, Output>) => A,
    ): A;
}

export interface HostOperations<_E> {}

export interface BinaryResource<_E> {
    readonly fileName?: string | undefined;
    asBytes(): AsyncIterable<Uint8Array>;
}

export type TestExecutionFailure = unknown;

