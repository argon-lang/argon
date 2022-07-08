package dev.argon.plugins.tube;

import dev.argon.plugin.api.*;
import dev.argon.plugin.api.tube.*;
import java.util.Set;

final class TubeBackend<E extends Exception> implements Backend<E, TubeOptions<E>, TubeOutput<E>> {
    @Override
    public TubeOutput<E> emitModule(TubeOptions<E> options, Set<Platform<E, ?, ?, ?>> platforms, SerializedTube<E> tube) {
        return new TubeOutput<E>(
            new SerializedTubeResource<E>(tube, platforms),
            new SerializedTubeResource<E>(new TubeInterface<E>(tube), platforms)
        );
    }
}
