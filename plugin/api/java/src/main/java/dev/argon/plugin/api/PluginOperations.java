package dev.argon.plugin.api;

import dev.argon.plugin.api.tube.ModulePath;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.concurrent.CancellationException;

public interface PluginOperations<E extends Throwable> {
    @NotNull RuntimeException wrapAsRuntimeException(@NotNull E ex);

    @NotNull RuntimeException wrapAsRuntimeException(@NotNull IOException ex);
    @NotNull RuntimeException wrapAsRuntimeException(@NotNull InterruptedException ex);
    
    @Nullable E asDomainException(@NotNull Throwable ex);
       
    
    
    public static interface TryBodyHandler<E extends Throwable, A> {
        @NotNull A run() throws E, IOException, InterruptedException;
    }
    
    <A> @NotNull A catchAsIOException(@NotNull TryBodyHandler<E, A> handler) throws IOException;
    default <A> @NotNull A catchAsRuntimeException(@NotNull TryBodyHandler<E, A> handler) {
        try {
            return catchAsIOException(handler);
        }
        catch(IOException ex) {
            throw wrapAsRuntimeException(ex);
        }
    }
    
}
