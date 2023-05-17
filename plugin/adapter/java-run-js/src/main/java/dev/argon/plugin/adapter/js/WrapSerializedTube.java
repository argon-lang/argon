package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.tube.SerializedTube;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;

sealed class WrapSerializedTube<E extends Throwable> permits WrapSerializedTubePlus {

    public WrapSerializedTube(JSEnv<E> env, SerializedTube<E> tube) {
        this.env = env;
        this.tube = tube;
    }

    protected final JSEnv<E> env;
    private final SerializedTube<E> tube;

    @HostAccess.Export
    public @NotNull Value version() {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var version = tube.version();

            env.lock.lock();
            try {
                return ProtoConverter.messageToJSProto(env, version);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value metadata() {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var metadata = tube.metadata();

            env.lock.lock();
            try {
                return ProtoConverter.messageToJSProto(env, metadata);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value getResource(@NotNull String id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var res = tube.getResource(id);
            return WrapFileSystemResource.wrap(env, res);
        });
    }

    @HostAccess.Export
    public @NotNull Value getClass(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var a = tube.getClass(id);

            env.lock.lock();
            try {
                return ProtoConverter.messageToJSProto(env, a);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value getTrait(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var a = tube.getTrait(id);

            env.lock.lock();
            try {
                return ProtoConverter.messageToJSProto(env, a);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value getFunction(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var a = tube.getFunction(id);

            env.lock.lock();
            try {
                return ProtoConverter.messageToJSProto(env, a);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value getMethod(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var a = tube.getMethod(id);

            env.lock.lock();
            try {
                return ProtoConverter.messageToJSProto(env, a);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value getClassConstructor(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            var a = tube.getClassConstructor(id);

            env.lock.lock();
            try {
                return ProtoConverter.messageToJSProto(env, a);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public @NotNull Value close() {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            tube.close();

            env.lock.lock();
            try {
                return env.context.eval("js", "undefined");
            }
            finally {
                env.lock.unlock();
            }
        });
    }
}
