package dev.argon.plugin.api;

import dev.argon.plugin.api.tube.SerializedTube;

import java.io.IOException;

public interface CloseableTube<E extends Exception> extends AutoCloseable {
    SerializedTube<E> tube();

    @Override
    void close() throws IOException, E, PluginException;
}
