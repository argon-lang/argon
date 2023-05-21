package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.nio.channels.SeekableByteChannel;
import java.util.Optional;


public non-sealed interface BinaryResource<E extends Throwable> extends FileSystemResource<E> {
    @NotNull InputStream asInputStream() throws E, IOException, InterruptedException;
    @NotNull Optional<SupplierWithError<E, @NotNull SeekableByteChannel>> asSeekableByteChannel();

    default void intoOutputStream(@NotNull OutputStream os) throws E, IOException, InterruptedException {
        try(var is = asInputStream()) {
            is.transferTo(os);
        }
    }
    
    @NotNull Optional<SupplierWithError<E, @NotNull BigInteger>> byteSize();
}
