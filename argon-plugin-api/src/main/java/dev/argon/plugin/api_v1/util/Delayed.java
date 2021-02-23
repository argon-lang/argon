package dev.argon.plugin.api_v1.util;

public interface Delayed<T> {
    T get() throws Exception;
}
