package dev.argon.plugin.api.options;

import java.io.IOException;

public interface OutputInfo<T, TOptions> {
    String name();
    String description();

    T getValue(TOptions options);    
}
