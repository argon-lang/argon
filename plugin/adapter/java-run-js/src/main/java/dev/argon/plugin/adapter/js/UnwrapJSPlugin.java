package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.Plugin;
import dev.argon.plugin.api.TubeLoader;
import dev.argon.plugin.api.options.OptionCodec;
import dev.argon.plugin.api.options.OutputHandler;
import dev.argon.plugin.api.tube.SerializedTubePlus;
import org.graalvm.polyglot.Value;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.jetbrains.annotations.NotNull; 

class UnwrapJSPlugin<E extends Throwable> implements Plugin<E, Value, Value, Value, Value, Value> {

    public UnwrapJSPlugin(JSEnv<E> env, Value jsPlugin) {
        this.env = env;
        this.jsPlugin = jsPlugin;
    }

    private final JSEnv<E> env;
    private final Value jsPlugin;

    @Override
    public @NotNull OptionCodec<E, Value> optionCodec() {
        Value jsCodec;

        env.lock.lock();
        try {
            jsCodec = jsPlugin.getMember("optionCodec");
        }
        finally {
            env.lock.unlock();
        }

        return new UnwrapOptionCodec<>(env, jsCodec);
    }

    @Override
    public @NotNull OutputHandler<E, Value> outputHandler() {
        Value jsOutputHandler;

        env.lock.lock();
        try {
            jsOutputHandler = jsPlugin.getMember("outputHandler");
        }
        finally {
            env.lock.unlock();
        }
        return new UnwrapOutputHandler<>(env, jsOutputHandler);
    }

    @Override
    public @NotNull Value emitTube(@NotNull Value options, @NotNull SerializedTubePlus<E, Value, Value, Value> tube) throws E, IOException, InterruptedException {
        var wrapTube = new WrapSerializedTubePlus<>(env, tube);

        Value output;
        env.lock.lock();
        try {
            output = jsPlugin.invokeMember("emitTube", options, wrapTube);
        }
        finally {
            env.lock.unlock();
        }

        return JavaFutureToJSPromise.runJSPromise(env, output);
    }

    @Override
    public @NotNull Optional<Value> loadExternMethod(@NotNull Value options, @NotNull String id) throws E, IOException, InterruptedException {
        Value extern;
        env.lock.lock();
        try {
            extern = jsPlugin.invokeMember("loadExternMethod", options, id);
        }
        finally {
            env.lock.unlock();
        }

        extern = JavaFutureToJSPromise.runJSPromise(env, extern);

        if(extern.isNull()) {
            return Optional.empty();
        }
        else {
            return Optional.of(extern);
        }
    }

    @Override
    public @NotNull Optional<Value> loadExternFunction(@NotNull Value options, @NotNull String id) throws E, IOException, InterruptedException {
        Value extern;
        env.lock.lock();
        try {
            extern = jsPlugin.invokeMember("loadExternFunction", options, id);
        }
        finally {
            env.lock.unlock();
        }

        extern = JavaFutureToJSPromise.runJSPromise(env, extern);

        if(extern.isNull()) {
            return Optional.empty();
        }
        else {
            return Optional.of(extern);
        }
    }

    @Override
    public @NotNull Optional<Value> loadExternClassConstructor(@NotNull Value options, @NotNull String id) throws E, IOException, InterruptedException {
        Value extern;
        env.lock.lock();
        try {
            extern = jsPlugin.invokeMember("loadExternClassConstructor", options, id);
        }
        finally {
            env.lock.unlock();
        }

        extern = JavaFutureToJSPromise.runJSPromise(env, extern);

        if(extern.isNull()) {
            return Optional.empty();
        }
        else {
            return Optional.of(extern);
        }
    }

    @Override
    public @NotNull Map<String, TubeLoader<E, ?>> tubeLoaders() {
        env.lock.lock();
        try {
            var loaders = jsPlugin.invokeMember("tubeLoaders");

            Map<String, TubeLoader<E, ?>> map = new HashMap<>();
            loaders.invokeMember("forEach", (MapForEachCallback)(key, value) -> map.put(key, UnwrapTubeLoader.fromFactory(env, value)));

            return map;
        }
        finally {
            env.lock.unlock();
        }
    }

    private static interface MapForEachCallback {
        void each(String key, Value value);
    }
}
