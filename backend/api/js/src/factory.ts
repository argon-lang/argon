import type { ErrorChecker } from "@argon-lang/noble-idl-core/util";
import type { Backend } from "./index.js";

export interface HostOperations<_E> {

}

export interface BackendFactory {
    create<E, A>(_errorChecker_e: ErrorChecker<E>, _hostOperations: HostOperations<E>, f: <Output>(backend: Backend<E, Output>) => A): A;
}
