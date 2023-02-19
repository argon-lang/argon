package dev.argon.plugin.api.options;

import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;

public interface OutputHandler<E extends Throwable, Output> {
    @NotNull Map<List<String>, OutputInfo<E, Output>> options();
}
