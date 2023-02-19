package dev.argon.plugin.api.options;

import dev.argon.plugin.api.ResourceFactory;
import dev.argon.plugin.api.tube.Toml;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public interface OptionDecoder<E extends Throwable, A> {
    @NotNull A decode(@NotNull ResourceFactory<E> resourceFactory, @NotNull Toml value) throws OptionDecodeException;
    default @NotNull Optional<A> defaultValue() {
        return Optional.empty();
    }
}
