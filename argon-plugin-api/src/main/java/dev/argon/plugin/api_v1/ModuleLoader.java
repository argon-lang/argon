package dev.argon.plugin.api_v1;

import org.jetbrains.annotations.NotNull;

public interface ModuleLoader {
    @NotNull ArgonModuleSerialized load(@NotNull String fileName) throws Exception;
}
