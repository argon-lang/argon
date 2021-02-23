package dev.argon.plugin.api_v1;

import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public interface ModuleLoader {
    @NotNull Optional<ArgonModuleSerialized> load(@NotNull String fileName) throws Exception;
}
