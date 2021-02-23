package dev.argon.plugin.api_v1.util;

@FunctionalInterface
public interface ExFunction<T, U> {
    U apply(T value) throws Exception;
}
