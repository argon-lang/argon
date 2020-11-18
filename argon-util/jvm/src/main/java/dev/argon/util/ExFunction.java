package dev.argon.util;

@FunctionalInterface
public interface ExFunction<T, U> {
    U apply(T value) throws Exception;
}
