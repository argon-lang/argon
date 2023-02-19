package dev.argon.plugin.api;

import dev.argon.plugin.api.tube.SerializedTube;
import dev.argon.plugin.api.tube.TubeName;
import org.jetbrains.annotations.NotNull;

public interface TubeImporter<E extends Throwable> {
    @NotNull SerializedTube<E> getTube(@NotNull TubeName tubeName) throws E;
}
