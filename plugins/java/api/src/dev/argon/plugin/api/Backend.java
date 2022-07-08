package dev.argon.plugin.api;

import java.util.Map;
import java.util.Set;
import dev.argon.plugin.api.resource.Resource;
import dev.argon.plugin.api.tube.SerializedTube;

public interface Backend<E extends Exception, TOptions, TOutput> {
    TOutput emitModule(TOptions options, Set<Platform<E, ?, ?, ?>> platforms, SerializedTube<E> tube);
}
