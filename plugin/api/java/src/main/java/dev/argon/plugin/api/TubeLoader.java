package dev.argon.plugin.api;

import dev.argon.plugin.api.options.OptionDecoder;
import dev.argon.plugin.api.tube.SerializedTube;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public interface TubeLoader<E extends Throwable, LibOptions> {
    @NotNull OptionDecoder<E, LibOptions> libOptionsDecoder();

    @NotNull SerializedTube<E> load(@NotNull TubeImporter<E> tubeImporter, @NotNull LibOptions libOptions) throws E, IOException, InterruptedException;
}
