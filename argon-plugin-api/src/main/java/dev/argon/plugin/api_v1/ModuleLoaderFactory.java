package dev.argon.plugin.api_v1;

import org.jetbrains.annotations.NotNull;

import java.util.Map;

public interface ModuleLoaderFactory {
    @NotNull ModuleLoader create(@NotNull Map<String, PluginOptionValue> options);
}
