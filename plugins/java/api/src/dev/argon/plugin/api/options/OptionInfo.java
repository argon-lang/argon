package dev.argon.plugin.api.options;

import java.io.IOException;

public sealed interface OptionInfo<E extends Exception, T, TOptions, TOptionsBuilder> permits OptionInfoValue, OptionInfoResource {
    String name();
    String description();

    T getValue(TOptions options);
}
