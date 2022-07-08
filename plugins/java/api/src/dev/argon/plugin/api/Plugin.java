package dev.argon.plugin.api;

import java.util.List;
import dev.argon.plugin.api.options.OptionHandler;
import dev.argon.plugin.api.options.OutputHandler;
import org.checkerframework.checker.nullness.qual.Nullable;

public interface Plugin<E extends Exception, TOptions, TOutput> {
    OptionHandler<E, TOptions, ?> optionHandler();
    OutputHandler<E, TOutput> outputHandler();
  
    Backend<E, TOptions, TOutput> backend();
    List<TubeLoader<E, TOptions>> tubeLoaders();
    @Nullable OutputExecutor<TOutput> outputExecutor();
}
