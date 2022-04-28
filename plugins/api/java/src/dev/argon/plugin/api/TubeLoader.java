package dev.argon.plugin.api;

import java.io.IOException;
import java.util.List;
import dev.argon.plugin.api.resource.BinaryResource;

public interface TubeLoader<E extends Exception, TOptions> {
    List<String> supportedExtensions();    
    CloseableTube<E> load(TOptions options, BinaryResource<E> resource) throws E, IOException, PluginException;
}
