package dev.argon.plugin.api.tube;

import dev.argon.plugin.api.FileSystemResource;
import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;

public interface SerializedTube<E extends Throwable> extends AutoCloseable {
    @NotNull TubeFormatVersion version() throws E;
    
    @NotNull Metadata metadata() throws E;

    @NotNull FileSystemResource<E> getResource(@NotNull String id) throws E;
    
    @NotNull ModuleDefinition getModule(@NotNull ModulePath id) throws E;
    @NotNull ClassDefinition getClass(BigInteger id) throws E;
    @NotNull TraitDefinition getTrait(BigInteger id) throws E;
    @NotNull FunctionDefinition getFunction(BigInteger id) throws E;
    @NotNull MethodDefinition getMethod(BigInteger id) throws E;
    @NotNull ClassConstructorDefinition getClassConstructor(BigInteger id) throws E;
}
