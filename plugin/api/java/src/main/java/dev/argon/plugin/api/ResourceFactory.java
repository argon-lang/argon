package dev.argon.plugin.api;

import dev.argon.plugin.api.options.OptionDecodeException;
import org.jetbrains.annotations.NotNull;
import dev.argon.plugin.api.tube.Toml;

public interface ResourceFactory<E extends Throwable> {
    @NotNull DirectoryResource<E, BinaryResource<E>> directoryResource(@NotNull String name) throws OptionDecodeException;
    @NotNull BinaryResource<E> binaryResource(@NotNull String name) throws OptionDecodeException;
}
