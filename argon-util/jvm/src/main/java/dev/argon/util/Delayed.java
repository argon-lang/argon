package dev.argon.util;

public interface Delayed<T> {
    T get() throws Exception;
}
