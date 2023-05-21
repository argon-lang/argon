package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.ResourceFactory;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

final class WrapResourceFactory<E extends Throwable> {

    public WrapResourceFactory(@NotNull JSEnv<E> env, @NotNull ResourceFactory<E> resourceFactory) {
        this.env = env;
        this.resourceFactory = resourceFactory;
    }

    private final JSEnv<E> env;
    private final ResourceFactory<E> resourceFactory;

    @HostAccess.Export
    public @NotNull Value directoryResource(@NotNull String id) throws InterruptedException {
        return WrapOptionDecodeResult.wrap(env, () -> {
            var res = new WrapDirectoryResource<E>(env, resourceFactory.directoryResource(id));
            env.lock.lockInterruptibly();
            try {
                return env.context.asValue(res);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value binaryResource(@NotNull String id) throws InterruptedException {
        return WrapOptionDecodeResult.wrap(env, () -> {
            var res = new WrapBinaryResource<E>(env, resourceFactory.binaryResource(id));
            env.lock.lockInterruptibly();
            try {
                return env.context.asValue(res);
            }
            finally {
                env.lock.unlock();
            }
        });
    }


    @FunctionalInterface
    public static interface ResourceFunction {
        @NotNull Value getResource(@NotNull String id);
    }

}
