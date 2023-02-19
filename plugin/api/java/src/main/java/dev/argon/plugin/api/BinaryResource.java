package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

import java.io.InputStream;
import java.nio.channels.SeekableByteChannel;
import java.util.Optional;


public non-sealed interface BinaryResource<E extends Throwable> extends FileSystemResource<E> {
    @NotNull InputStream asInputStream() throws E;
    @NotNull Optional<SupplierWithError<E, @NotNull SeekableByteChannel>> asSeekableByteChannel();
}
