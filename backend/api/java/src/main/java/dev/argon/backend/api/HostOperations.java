package dev.argon.backend.api;

import java.io.IOException;

public interface HostOperations<E> {
	E errorFromIOException(IOException ex);
}
