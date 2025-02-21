import * as backendApi from "@argon-lang/js-backend-api";

export interface JSBackendOptions<E> {
    readonly externs: readonly backendApi.BinaryResource<E>[];
}

export interface JSBackendOutput<E> {
    readonly modules: backendApi.DirectoryResource<E>;
}
