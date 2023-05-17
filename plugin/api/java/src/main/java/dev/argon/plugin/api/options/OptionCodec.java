package dev.argon.plugin.api.options;

import dev.argon.plugin.api.ResourceRecorder;
import dev.argon.plugin.api.tube.Toml;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public interface OptionCodec<E extends Throwable, A> extends OptionDecoder<E, A> {
    @NotNull Toml encode(@NotNull ResourceRecorder<E> recorder, @NotNull A value) throws E, IOException, InterruptedException;
}
