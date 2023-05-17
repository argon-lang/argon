package dev.argon.plugin.api.tube;

import dev.argon.plugin.api.FileSystemResource;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.math.BigInteger;

public interface SerializedTube<E extends Throwable> extends AutoCloseable {
    @NotNull TubeFormatVersion version() throws E, IOException, InterruptedException;
    
    @NotNull Metadata metadata() throws E, IOException, InterruptedException;

    @NotNull FileSystemResource<E> getResource(@NotNull String id) throws E, IOException, InterruptedException;
    
    @NotNull ModuleDefinition getModule(@NotNull ModulePath id) throws E, IOException, InterruptedException;
    @NotNull ClassDefinition getClass(BigInteger id) throws E, IOException, InterruptedException;
    @NotNull TraitDefinition getTrait(BigInteger id) throws E, IOException, InterruptedException;
    @NotNull FunctionDefinition getFunction(BigInteger id) throws E, IOException, InterruptedException;
    @NotNull MethodDefinition getMethod(BigInteger id) throws E, IOException, InterruptedException;
    @NotNull ClassConstructorDefinition getClassConstructor(BigInteger id) throws E, IOException, InterruptedException;
}
