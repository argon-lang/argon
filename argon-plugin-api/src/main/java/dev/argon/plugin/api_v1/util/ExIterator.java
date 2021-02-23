package dev.argon.plugin.api_v1.util;

import org.jetbrains.annotations.Nullable;

public interface ExIterator<T> extends AutoCloseable {
    @Nullable T next() throws Exception;
}
