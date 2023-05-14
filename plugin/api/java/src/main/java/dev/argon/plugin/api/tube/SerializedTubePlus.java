package dev.argon.plugin.api.tube;

import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;

public interface SerializedTubePlus<E extends Throwable, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation> extends SerializedTube<E> {
    @NotNull ExternMethodImplementation getExternMethodImplementation(@NotNull BigInteger id) throws E;
    @NotNull ExternFunctionImplementation getExternFunctionImplementation(@NotNull BigInteger id) throws E;
    @NotNull ExternClassConstructorImplementation getExternClassConstructorImplementation(@NotNull BigInteger id) throws E;

    @NotNull VTable getVTableDiff(@NotNull BigInteger id) throws E;
}
