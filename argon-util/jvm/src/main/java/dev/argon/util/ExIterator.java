package dev.argon.util;

public interface ExIterator<T> extends AutoCloseable {
    T next() throws Exception;
}
