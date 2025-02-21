import type * as backendApi from "@argon-lang/js-backend-api";
import type { Option } from "@argon-lang/esexpr";
import type { JSBackendOutput } from "../options.js";


export function createTestExecutor<E>(): Option<backendApi.TestExecutorFactory<E, JSBackendOutput<E>>> {
    return null;
}

