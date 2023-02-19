package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

public interface ResourceFactory<E extends Throwable> {
    @NotNull DirectoryResource<E, BinaryResource<E>> directoryResource(@NotNull String name);
    @NotNull BinaryResource<E> binaryResource(@NotNull String name);
}
