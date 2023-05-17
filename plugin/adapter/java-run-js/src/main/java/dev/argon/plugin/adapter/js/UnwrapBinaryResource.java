package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.BinaryResource;
import dev.argon.plugin.api.SupplierWithError;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.io.InputStream;
import java.math.BigInteger;
import java.nio.channels.SeekableByteChannel;
import java.util.Optional;

public class UnwrapBinaryResource<E extends Throwable> implements BinaryResource<E> {

    public UnwrapBinaryResource(JSEnv<E> env, Value res) {
        this.env = env;

        this.res = res;
    }

    private final JSEnv<E> env;
    private final Value res;

    @Override
    public @NotNull InputStream asInputStream() throws E {
        env.lock.lock();
        try {
            return InputStreamAsAsyncIterable.fromAsyncIterable(env, res.invokeMember("asAsyncIterable"));
        }
        finally {
            env.lock.unlock();
        }
    }

    @Override
    public @NotNull Optional<SupplierWithError<E, @NotNull SeekableByteChannel>> asSeekableByteChannel() {
        return Optional.empty();
    }

    @Override
    public @NotNull Optional<SupplierWithError<E, @NotNull BigInteger>> byteSize() {
        env.lock.lock();
        try {
            var byteSizeFunc = res.getMember("byteSize");
            if(byteSizeFunc.isNull()) {
                return Optional.empty();
            }

            SupplierWithError<E, @NotNull BigInteger> func = () -> {
                Value funcRes;
                env.lock.lock();
                try {
                    funcRes = byteSizeFunc.execute();
                }
                finally {
                    env.lock.unlock();
                }
                return JavaFutureToJSPromise.runJSPromise(env, funcRes).as(BigInteger.class);
            };

            return Optional.of(func);
        }
        finally {
            env.lock.unlock();
        }
    }

    @Override
    public @NotNull Optional<String> fileName() {
        env.lock.lock();
        try {
            var fileName = res.getMember("fileName");
            if(fileName.isNull()) {
                return Optional.empty();
            }

            return Optional.of(fileName.asString());
        }
        finally {
            env.lock.unlock();
        }
    }
}
