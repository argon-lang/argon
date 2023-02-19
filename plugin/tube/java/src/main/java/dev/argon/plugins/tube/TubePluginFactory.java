package dev.argon.plugins.tube;

import dev.argon.plugin.api.Plugin;
import dev.argon.plugin.api.PluginFactory;
import dev.argon.plugin.api.PluginOperations;
import org.jetbrains.annotations.NotNull;

public class TubePluginFactory implements PluginFactory {
    @Override
    public @NotNull <E extends Throwable> Plugin<E, ?, ?, ?, ?, ?> create(PluginOperations<E> operations) {
//        new TubeOptionsBuilder();
        throw new RuntimeException("not implemented");
    }
}
