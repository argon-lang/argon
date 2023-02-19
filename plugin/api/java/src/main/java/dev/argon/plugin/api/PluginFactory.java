package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

public interface PluginFactory {
    <E extends Throwable> @NotNull Plugin<E, ?, ?, ?, ?, ?> create(@NotNull PluginOperations<E> operations);
}
