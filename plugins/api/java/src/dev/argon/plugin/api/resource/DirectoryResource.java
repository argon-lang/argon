package dev.argon.plugin.api.resource;

import dev.argon.plugin.api.*;
import java.util.stream.Stream;
import java.io.IOException;

public abstract non-sealed class DirectoryResource<E extends Exception> extends Resource<E> {
    public abstract Stream<DirectoryEntry<E>> contents() throws IOException, E, PluginException;
}
