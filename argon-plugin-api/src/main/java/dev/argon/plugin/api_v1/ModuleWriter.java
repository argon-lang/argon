package dev.argon.plugin.api_v1;

import org.jetbrains.annotations.NotNull;

import java.io.OutputStream;

public interface ModuleWriter {
    void writeModule(@NotNull ArgonModuleSerialized module, @NotNull OutputStream stream) throws Exception;
}
