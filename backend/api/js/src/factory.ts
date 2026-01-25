import type { ErrorChecker } from "@argon-lang/noble-idl-core/util";
import type { Backend } from "./index.js";
import type {BackendMetadata} from "./metadata.js";

export interface HostOperations<_E> {

}

export interface SimpleBackendFactory {
    create<E, A>(_errorChecker_e: ErrorChecker<E>, _hostOperations: HostOperations<E>, f: <Output>(backend: Backend<E, Output>) => A): A;
}

export interface BackendFactory extends SimpleBackendFactory {
    readonly metadata: BackendMetadata;
    close(): Promise<void>;
}
