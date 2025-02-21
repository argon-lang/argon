package dev.argon.backend.api;

import dev.argon.nobleidl.runtime.ErrorType;

public interface BackendFactory {
	<TE, EE extends Throwable> Backend<TE, ?> create(ErrorType<TE, EE> errorType, HostOperations<TE> hostOperations) throws InterruptedException;
}
