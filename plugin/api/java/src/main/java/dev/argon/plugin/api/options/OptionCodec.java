package dev.argon.plugin.api.options;

import dev.argon.plugin.api.ResourceRecorder;
import dev.argon.plugin.api.tube.Toml;
import org.jetbrains.annotations.NotNull;

public interface OptionCodec<E extends Throwable, A> extends OptionDecoder<E, A> {
    @NotNull Toml encode(@NotNull ResourceRecorder<E> recorder, @NotNull A value) throws E;
    default boolean skipForField(@NotNull A value) {
        return false;
    }
}
