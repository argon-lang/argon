package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;

public interface ResourceRecorder<E extends Throwable> {
    @NotNull String recordBinaryResource(BinaryResource<E> resource) throws E;
    @NotNull String recordDirectoryResource(DirectoryResource<E, BinaryResource<E>> resource) throws E;
}
