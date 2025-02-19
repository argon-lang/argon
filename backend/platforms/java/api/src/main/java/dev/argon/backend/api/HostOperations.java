package dev.argon.backend.api;

import java.io.IOException;

public interface HostOperations<E> {
	IOException errorToIOException(E error);
	E errorFromIOException(IOException ex);
}
