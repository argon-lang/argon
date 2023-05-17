package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.BinaryResource;
import dev.argon.plugin.api.DirectoryEntry;
import dev.argon.plugin.api.DirectoryResource;
import dev.argon.plugin.api.SupplierWithError;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.io.InputStream;
import java.math.BigInteger;
import java.nio.channels.SeekableByteChannel;
import java.util.Optional;
import java.util.stream.Stream;

public class UnwrapDirectoryResource<E extends Throwable> implements DirectoryResource<E, BinaryResource<E>> {

    public UnwrapDirectoryResource(JSEnv<E> env, Value res) {
        this.env = env;
        this.res = res;
    }

    private final JSEnv<E> env;
    private final Value res;

    @Override
    public @NotNull Stream<DirectoryEntry<E, BinaryResource<E>>> contents() throws E {
        Value iter;
        env.lock.lock();
        try {
            iter = res.invokeMember("contents");
        }
        finally {
            env.lock.unlock();
        }

        return StreamAsAsyncIterable.fromAsyncIterable(env, iter).map(this::toDirectoryEntry);
    }

    private DirectoryEntry<E, BinaryResource<E>> toDirectoryEntry(Value entry) {
        env.lock.lock();
        try {
            return switch(entry.getMember("entryType").asString()) {
                case "file" -> new DirectoryEntry.File<>(
                    entry.getMember("name").asString(),
                    new UnwrapBinaryResource<>(env, entry.getMember("resource"))
                );
                
                case "directory" -> new DirectoryEntry.Subdirectory<>(
                    entry.getMember("name").asString(),
                    new UnwrapDirectoryResource<>(env, entry.getMember("resource"))
                );
                
                default -> throw new RuntimeException("Unexpected entryType");
            };
        }
        finally {
            env.lock.unlock();
        }
    }

    @Override
    public @NotNull Optional<SupplierWithError<E, @NotNull BigInteger>> numEntries() {
        env.lock.lock();
        try {
            var numEntriesFunc = res.getMember("numEntries");
            if(numEntriesFunc.isNull()) {
                return Optional.empty();
            }

            SupplierWithError<E, @NotNull BigInteger> func = () -> {
                Value funcRes;
                env.lock.lock();
                try {
                    funcRes = numEntriesFunc.execute();
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
