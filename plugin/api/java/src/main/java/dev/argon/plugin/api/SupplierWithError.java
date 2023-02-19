package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

@FunctionalInterface
public interface SupplierWithError<E extends Throwable, T> {
    @NotNull T get() throws E;
}
