package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;
import java.util.Optional;
import java.util.stream.Stream;

public non-sealed interface DirectoryResource<E extends Throwable, FileResource extends BinaryResource<E>> extends FileSystemResource<E> {
    @NotNull Stream<DirectoryEntry<E, FileResource>> contents() throws E;
    
    @NotNull Optional<SupplierWithError<E, @NotNull BigInteger>> numEntries();
}
