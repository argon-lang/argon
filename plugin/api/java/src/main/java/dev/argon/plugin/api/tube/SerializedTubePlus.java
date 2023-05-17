package dev.argon.plugin.api.tube;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.math.BigInteger;

public interface SerializedTubePlus<E extends Throwable, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation> extends SerializedTube<E> {
    @NotNull ExternMethodImplementation getExternMethodImplementation(@NotNull BigInteger id) throws E, IOException, InterruptedException;
    @NotNull ExternFunctionImplementation getExternFunctionImplementation(@NotNull BigInteger id) throws E, IOException, InterruptedException;
    @NotNull ExternClassConstructorImplementation getExternClassConstructorImplementation(@NotNull BigInteger id) throws E, IOException, InterruptedException;

    @NotNull VTable getVTableDiff(@NotNull BigInteger id) throws E, IOException, InterruptedException;
}
