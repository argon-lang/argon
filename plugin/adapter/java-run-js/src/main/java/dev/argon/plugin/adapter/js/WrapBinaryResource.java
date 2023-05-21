package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.BinaryResource;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;

final class WrapBinaryResource<E extends Throwable> {

    public WrapBinaryResource(@NotNull JSEnv<E> env, BinaryResource<E> binaryResource) {
        this.env = env;
        this.binaryResource = binaryResource;
        fileName = binaryResource.fileName().orElse(null);
    }


    private final JSEnv<E> env;
    private final BinaryResource<E> binaryResource;


    @HostAccess.Export
    public final String resourceType = "binary";

    @HostAccess.Export
    public final @Nullable String fileName;

    @HostAccess.Export
    public Value asAsyncIterable() throws E, java.io.IOException, InterruptedException {
        return InputStreamAsAsyncIterable.toAsyncIterable(env, binaryResource::asInputStream);
    }

    @HostAccess.Export
    public @Nullable GetByteSizeFunction<E> byteSize() {
        if(binaryResource.byteSize().isEmpty()) {
            return null;
        }
        else {
            return () -> {
                var size = binaryResource.byteSize().get().get();

                env.lock.lockInterruptibly();
                try {
                    return env.context.eval("js", "BigInt").execute(size.toString());
                }
                finally {
                    env.lock.unlock();
                }
            };
        }
    }

    @FunctionalInterface
    public static interface GetByteSizeFunction<E extends Throwable> {
        Value get() throws E, IOException, InterruptedException;
    }

}
