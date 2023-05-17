package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.math.BigInteger;

public interface ResourceRecorder<E extends Throwable> {
    @NotNull String recordBinaryResource(BinaryResource<E> resource) throws E, IOException, InterruptedException;
    @NotNull String recordDirectoryResource(DirectoryResource<E, BinaryResource<E>> resource) throws E, IOException, InterruptedException;
}
