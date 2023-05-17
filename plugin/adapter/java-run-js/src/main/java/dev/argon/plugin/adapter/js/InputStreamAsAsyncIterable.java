package dev.argon.plugin.adapter.js;

import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

final class InputStreamAsAsyncIterable {
    private InputStreamAsAsyncIterable() {}

    public static <E extends Throwable> Value toAsyncIterable(JSEnv<E> env, CreateInputStream<E> createIS) {
        return StreamAsAsyncIterable.toAsyncIterable(env, () -> {
            var is = createIS.create();

            var isIter = new Iterator<Value>() {


                private boolean done = false;
                private byte @Nullable[] nextValue = null;

                private void ensureNextValue() {
                    try {
                        if(!done && nextValue != null) {
                            byte[] b = new byte[4096];
                            int len = is.read(b);
                            if(len == 0) {
                                done = true;

                            }
                            else if(len < b.length) {
                                nextValue = Arrays.copyOf(b, len);
                            }
                            else {
                                nextValue = b;
                            }
                        }
                    }
                    catch(IOException ex) {
                        throw env.pluginOperations.wrapAsRuntimeException(ex);
                    }
                }

                @Override
                public boolean hasNext() {
                    return false;
                }

                @Override
                public Value next() {
                    ensureNextValue();
                    if(done) {
                        throw new NoSuchElementException();
                    }

                    Value arr;
                    env.lock.lock();
                    try {
                        arr = env.context.eval("js", "Uint8Array").execute(env.context.asValue(nextValue));
                    }
                    finally {
                        env.lock.unlock();
                    }

                    return arr;
                }
            };

            return StreamSupport
                .stream(Spliterators.spliteratorUnknownSize(isIter, 0), false)
                .onClose(() -> {
                    try {
                        is.close();
                    }
                    catch(IOException ex) {
                        throw env.pluginOperations.wrapAsRuntimeException(ex);
                    }
                });
        });
    }

    public static <E extends Throwable> InputStream fromAsyncIterable(JSEnv<E> env, Value asyncIterable) {
        return new IterableStream<E>(env, StreamAsAsyncIterable.fromAsyncIterable(env, asyncIterable));
    }

    private static class IterableStream<E extends Throwable> extends InputStream {

        public IterableStream(JSEnv<E> env, @NotNull Stream<@NotNull Value> stream) {
            this.env = env;
            this.stream = stream;
            iter = stream.iterator();
        }

        private final JSEnv<E> env;
        private final @NotNull Stream<@NotNull Value> stream;
        private final @NotNull Iterator<@NotNull Value> iter;

        private int remainingBufferPos = 0;
        private byte @Nullable[] remainingBuffer = null;

        @Override
        public int read() throws IOException {
            byte[] b = new byte[1];
            int len = read(b);
            if(len == 0) {
                return -1;
            }
            else {
                return b[0];
            }
        }

        @Override
        public int read(byte @NotNull[] b, int off, int len) throws IOException {
            Objects.checkFromIndexSize(off, len, b.length);

            if(remainingBuffer != null) {
                if(len > remainingBuffer.length - remainingBufferPos) {
                    len = remainingBuffer.length - remainingBufferPos;
                }

                System.arraycopy(remainingBuffer, off, b, off, len);
                remainingBufferPos += len;
                if(remainingBufferPos >= remainingBuffer.length) {
                    remainingBuffer = null;
                    remainingBufferPos = 0;
                }
                return len;
            }

            while(iter.hasNext()) {
                var arr = iter.next();
                
                env.lock.lock();
                try {
                    if(arr.getArraySize() == 0) {
                        continue;
                    }

                    if(arr.getArraySize() > Integer.MAX_VALUE) {
                        throw new RuntimeException("Array size is too large");
                    }

                    len = Math.min(len, (int)arr.getArraySize());
                    for(int i = 0; i < len; ++i) {
                        b[off + i] = (byte)arr.getArrayElement(i).asShort();
                    }

                    if(len < arr.getArraySize()) {
                        remainingBuffer = new byte[(int)arr.getArraySize() - len];
                        remainingBufferPos = 0;
                        for(int i = 0; i < remainingBuffer.length; ++i) {
                            remainingBuffer[i] = (byte)arr.getArrayElement(len + i).asShort();
                        }
                    }
                }
                finally {
                    env.lock.unlock();
                }
            }

            return 0;
        }

        @Override
        public void close() throws IOException {
            stream.close();
        }
    }

    private static interface NextFunction<E extends Throwable> {
        byte[] next() throws E, IOException;
    }

    @FunctionalInterface
    public static interface CreateInputStream<E extends Throwable> {
        InputStream create() throws E, IOException, InterruptedException;
    }
}
