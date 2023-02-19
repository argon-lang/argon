package dev.argon.plugin.api.options;

import dev.argon.plugin.api.FileSystemResource;
import org.jetbrains.annotations.NotNull;

public interface OutputInfo<E extends Throwable, Output> {
    @NotNull FileSystemResource<E> getValue(@NotNull Output output);
}
