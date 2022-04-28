package dev.argon.plugin.api;

import dev.argon.plugin.api.resource.*;

public interface ExternCodec<E extends Exception, T> {
    T decode(BinaryResource<E> resource) throws E;
    BinaryResource<E> encode(T value);
}
