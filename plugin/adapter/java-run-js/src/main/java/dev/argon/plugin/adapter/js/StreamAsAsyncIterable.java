package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.SupplierWithError;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyObject;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

final class StreamAsAsyncIterable {
    private StreamAsAsyncIterable() {}

    public static <E extends Throwable> Value toAsyncIterable(@NotNull JSEnv<E> env, @NotNull CreateStream<E> createStream) {


        SupplierWithError<E, ProxyObject> createFuncs = () -> {
            final Stream<Value> stream = createStream.create();
            Spliterator<@NotNull Value> iter = stream.spliterator();


            NextFunction<E> next = () -> {
                var advanceHandler = new Consumer<Value>() {
                    public @Nullable Value res = null;

                    @Override
                    public void accept(Value value) {
                        res = value;
                    }
                };

                iter.tryAdvance(advanceHandler);

                if(advanceHandler.res != null) {
                    return env.context.eval("js", "value => ({ done: false, value })");
                }
                else {
                    return null;
                }
            };

            CloseFunction<E> close = stream::close;

            return ProxyObject.fromMap(Map.of(
                "next", next,
                "close", close
            ));
        };


        env.lock.lock();
        try {
            return env.context.eval("js",
                "createFuncs => ({" +
                    "    async *[Symbol.asyncIterator]() {" +
                    "        const { next, close } = createFuncs();" +
                    "        try {" +
                    "            while(true) {" +
                    "                const value = await next();" +
                    "                if(value.done) {" +
                    "                    break;" +
                    "                }" +
                    "                yield value.value;" +
                    "            }" +
                    "        }" +
                    "        finally {" +
                    "            close();" +
                    "        }" +
                    "    }" +
                    "})").execute(createFuncs);
        }
        finally {
            env.lock.unlock();
        }
    }

    public static <E extends Throwable> @NotNull Stream<@NotNull Value> fromAsyncIterable(JSEnv<E> env, Value asyncIterable) {
        Value asyncIter;
        env.lock.lock();
        try {
            asyncIter = env.context.eval("js", "iterator => iterator[Symbol.asyncIterator]()").execute(asyncIterable);
        }
        finally {
            env.lock.unlock();
        }

        var iter = new UnwrapAsyncIterator<>(env, asyncIter);

        return StreamSupport
            .stream(Spliterators.spliteratorUnknownSize(iter, 0), false)
            .onClose(() -> asyncIter.invokeMember("return"));
    }

    private static class UnwrapAsyncIterator<E extends Throwable> implements Iterator<@NotNull Value> {

        public UnwrapAsyncIterator(JSEnv<E> env, Value asyncIter) {
            this.env = env;
            this.asyncIter = asyncIter;
        }

        private final JSEnv<E> env;
        private final Value asyncIter;

        private boolean done = false;
        private @Nullable Value nextValue = null;

        private void ensureNextValue() {
            if(!done && nextValue == null) {
                Value nextRes;
                env.lock.lock();
                try {
                    nextRes = asyncIter.invokeMember("next");
                }
                finally {
                    env.lock.unlock();
                }

                var res = env.pluginOperations.catchAsRuntimeException(() -> JavaFutureToJSPromise.runJSPromise(env, nextRes));

                env.lock.lock();
                try {
                    if(res.getMember("done").asBoolean()) {
                        done = true;
                    }
                    else {
                        nextValue = res.getMember("value");
                    }
                }
                finally {
                    env.lock.unlock();
                }
            }
        }

        @Override
        public boolean hasNext() {
            ensureNextValue();
            return nextValue != null;
        }

        @Override
        public Value next() {
            ensureNextValue();
            if(done) {
                throw new NoSuchElementException();
            }
            
            var res = nextValue;
            nextValue = null;
            return res;
        }
    }

    private static interface NextFunction<E extends Throwable> {
        Value next() throws E, IOException, InterruptedException;
    }

    private static interface CloseFunction<E extends Throwable> {
        void close() throws E, IOException, InterruptedException;
    }

    @FunctionalInterface
    public static interface CreateStream<E extends Throwable> {
        @NotNull Stream<@NotNull Value> create() throws E, IOException, InterruptedException;
    }
}
