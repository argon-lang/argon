package dev.argon.plugins.tube;

import java.util.List;
import dev.argon.plugin.api.*;
import dev.argon.plugin.api.options.*;
import org.checkerframework.checker.nullness.qual.*;

class PluginImpl<E extends Exception> implements Plugin<E, TubeOptions<E>, TubeOutput<E>> {
    @Override
    public OptionHandler<E, TubeOptions<E>, ?> optionHandler() {
        return new TubeOptionsHandler<E>();
    }

    @Override
    public OutputHandler<E, TubeOutput<E>> outputHandler() {
        return new TubeOutputHandler<E>();
    }

    @Override
    public Backend<E, TubeOptions<E>, TubeOutput<E>> backend() {
        return new TubeBackend<E>();
    }

    @Override
    public List<TubeLoader<E, TubeOptions<E>>> tubeLoaders() {
        return List.of(
            new TubeZipLoader<E>()
        );
    }

    @Override
    public @Nullable OutputExecutor<TubeOutput<E>> outputExecutor() {
        return null;
    }
}
