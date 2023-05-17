package dev.argon.plugin.adapter.js;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

final class ErrorUtil {
    private ErrorUtil() {}

    public static <E extends Throwable, A> A unwrapFutureValue(JSEnv<E> env, Future<A> future) throws E, IOException, InterruptedException {
        try {
            return future.get();
        }
        catch (ExecutionException ex) {
            var domainEx = env.pluginOperations.asDomainException(ex.getCause());
            if(domainEx != null) {
                throw domainEx;
            }

            if(ex.getCause() instanceof InterruptedException exInt) {
                throw exInt;
            }

            if(ex.getCause() instanceof IOException exIO) {
                throw exIO;
            }

            throw new RuntimeException(ex.getCause());
        }
    }
    
    public static <E extends Throwable, A> A unwrapFutureValueIO(JSEnv<E> env, Future<A> future) throws IOException {
        return env.pluginOperations.catchAsIOException(() -> unwrapFutureValue(env, future));
    }

}
