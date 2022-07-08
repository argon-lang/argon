package dev.argon.plugins.tube;

import dev.argon.plugin.api.tube.*;
import dev.argon.plugin.api.Platform;
import java.io.IOException;
import java.util.Set;

final class SerializedTubeResource<E extends Exception> extends TubeResource<E> {

    public SerializedTubeResource(SerializedTube<E> tube, Set<Platform<E, ?, ?, ?>> platforms) {
        super(platforms);
        this.tube = tube;
    }

    private final SerializedTube<E> tube;

    @Override
    public SerializedTube<E> asTube() throws IOException, E {
        return tube;
    }
}
