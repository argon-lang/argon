package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.TubeImporter;
import dev.argon.plugin.api.TubeLoader;
import dev.argon.plugin.api.options.OptionDecoder;
import dev.argon.plugin.api.tube.SerializedTube;
import dev.argon.plugin.api.tube.Toml;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

final class UnwrapTubeLoader<E extends Throwable> implements TubeLoader<E, Value> {

    public UnwrapTubeLoader(@NotNull JSEnv<E> env, @NotNull Value tubeLoader) {
        this.env = env;
        this.tubeLoader = tubeLoader;
    }

    private final JSEnv<E> env;
    private final Value tubeLoader;

    @Override
    public @NotNull OptionDecoder<E, Value> libOptionsDecoder() {
        try {
            Value decoder;

            env.lock.lockInterruptibly();
            try {
                decoder = tubeLoader.getMember("libOptionsDecoder");
            }
            finally {
                env.lock.unlock();
            }

            return new UnwrapOptionDecoder<>(env, decoder);
        }
        catch(InterruptedException ex) {
            throw env.pluginOperations.wrapAsRuntimeException(ex);
        }
    }

    @Override
    public @NotNull SerializedTube<E> load(@NotNull TubeImporter<E> tubeImporter, @NotNull Value libOptions) throws E, IOException, InterruptedException {
        Value tube;
        env.lock.lockInterruptibly();
        try {
            tube = tubeLoader.invokeMember("load", new WrapTubeImporter<>(env, tubeImporter), libOptions);
        }
        finally {
            env.lock.unlock();
        }

        tube = JavaFutureToJSPromise.runJSPromise(env, tube);

        return new UnwrapSerializedTube<>(env, tube);
    }

    public static <E extends Throwable> TubeLoader<E, ?> fromFactory(JSEnv<E> env, Value value) {
        try {
            env.lock.lockInterruptibly();
            try {
                return new UnwrapTubeLoader<>(env, value.invokeMember("withLoader", new LoaderConsumerImpl()));
            }
            finally {
                env.lock.unlock();
            }
        }
        catch(InterruptedException ex) {
            throw env.pluginOperations.wrapAsRuntimeException(ex);
        }
    }

    private static final class LoaderConsumerImpl {
        @HostAccess.Export
        public Value consume(Value loader) {
            return loader;
        }
    }
}
