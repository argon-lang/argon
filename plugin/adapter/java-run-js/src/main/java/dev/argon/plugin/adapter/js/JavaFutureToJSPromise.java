package dev.argon.plugin.adapter.js;

import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;

final class JavaFutureToJSPromise {
    private JavaFutureToJSPromise() {}


    public static @NotNull Value executeAsPromise(@NotNull JSEnv<?> env, @NotNull Execution execution) {
        CompletableFuture<Value> future = new CompletableFuture<>();
        env.executor.execute(() -> {
            Value value;
            try {
                value = execution.run();
            }
            catch(Throwable ex) {
                future.completeExceptionally(ex);
                return;
            }
            future.complete(value);
        });
        
        return toJSPromise(env, future);
    }

    public static @NotNull Value toJSPromise(@NotNull JSEnv<?> env, @NotNull Future<Value> future) {
        CreatePromiseFunction createPromise = (resolve, reject) -> {
            env.executor.execute(() -> {
                Value value;
                try {
                    value = future.get();
                }
                catch(ExecutionException ex) {
                    env.lock.lock();
                    try {
                        reject.executeVoid(ex.getCause());
                    }
                    finally {
                        env.lock.unlock();
                    }
                    return;
                }
                catch(Exception ex) {
                    env.lock.lock();
                    try {
                        reject.executeVoid(ex);
                    }
                    finally {
                        env.lock.unlock();
                    }
                    return;
                }


                env.lock.lock();
                try {
                    resolve.executeVoid(value);
                }
                finally {
                    env.lock.unlock();
                }
            });
        };


        try {
            env.lock.lockInterruptibly();
            try {
                return env.context.eval("js", "createPromise => new Promise(createPromise)").execute(createPromise);
            }
            finally {
                env.lock.unlock();
            }
        }
        catch(InterruptedException ex) {
            throw env.pluginOperations.wrapAsRuntimeException(ex);
        }
    }

    public static @NotNull Future<Value> fromJSPromise(@NotNull JSEnv<?> env, @NotNull Value promise) throws InterruptedException {
        var future = new CompletableFuture<Value>();

        PromiseThenFunc onFulfilled = future::complete;
        PromiseThenFunc onRejected = error -> {
            env.lock.lock();
            try {
                if(error.isException()) {
                    try {
                        throw error.throwException();
                    }
                    catch(Throwable ex) {
                        future.completeExceptionally(ex);
                    }
                }
                else {
                    throw new JavaScriptException(error);
                }
            }
            finally {
                env.lock.unlock();
            }
        };

        env.lock.lockInterruptibly();
        try {
            promise.invokeMember("then", onFulfilled, onRejected);
        }
        finally {
            env.lock.unlock();
        }

        return future;
    }
    
    public static <E extends Throwable> @NotNull Value runJSPromise(@NotNull JSEnv<E> env, @NotNull Value promise) throws E, IOException, InterruptedException {
        return ErrorUtil.unwrapFutureValue(env, fromJSPromise(env, promise));
    }


    @FunctionalInterface
    private static interface CreatePromiseFunction {
        void create(Value resolve, Value reject);
    }


    @FunctionalInterface
    private static interface PromiseThenFunc {
        void handle(Value value);
    }

    @FunctionalInterface
    public static interface Execution {
        Value run() throws Throwable;
    }

}
