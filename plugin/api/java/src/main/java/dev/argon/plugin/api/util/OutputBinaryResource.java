package dev.argon.plugin.api.util;

import dev.argon.plugin.api.BinaryResource;
import dev.argon.plugin.api.PluginOperations;
import dev.argon.plugin.api.SupplierWithError;
import org.jetbrains.annotations.NotNull;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.nio.channels.SeekableByteChannel;
import java.util.Optional;

public abstract class OutputBinaryResource<E extends Throwable> implements BinaryResource<E> {

    public OutputBinaryResource(PluginOperations<E> pluginOperations) {
        this.pluginOperations = pluginOperations;
    }

    protected final PluginOperations<E> pluginOperations;

    @Override
    public abstract void intoOutputStream(@NotNull OutputStream os) throws E, IOException, InterruptedException;

    @Override
    public @NotNull InputStream asInputStream() throws E, IOException, InterruptedException {
        var is = new ConnectedInputStream(pluginOperations);

        new Thread(() -> {
            var os = is.new ConnectedOutputStream();
            try {
                intoOutputStream(os);
            }
            catch(Throwable ex) {
                try {
                    os.setException(ex);
                }
                catch(InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
            finally {
                os.close();
            }
        }).start();

        return is;
    }

    @Override
    public @NotNull Optional<SupplierWithError<E, @NotNull SeekableByteChannel>> asSeekableByteChannel() {
        return Optional.empty();
    }

    @Override
    public @NotNull Optional<SupplierWithError<E, @NotNull BigInteger>> byteSize() {
        return Optional.empty();
    }
}
