package dev.argon.backend.api;

import dev.argon.backend.api.metadata.BackendMetadata;
import dev.argon.nobleidl.runtime.ErrorType;

public interface BackendFactory extends SimpleBackendFactory, AutoCloseable {
	BackendMetadata metadata();
}
