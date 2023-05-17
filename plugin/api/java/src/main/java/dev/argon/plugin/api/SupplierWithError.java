package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;

@FunctionalInterface
public interface SupplierWithError<E extends Throwable, T> {
    @NotNull T get() throws E, IOException, InterruptedException;
}
