package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.FileSystemResource;
import dev.argon.plugin.api.tube.*;
import dev.argon.plugin.api.tube.TubeFormatVersion;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.math.BigInteger;

public class UnwrapSerializedTube<E extends Throwable> implements SerializedTube<E> {

    public UnwrapSerializedTube(@NotNull JSEnv<E> env, @NotNull Value tube) {
        this.env = env;
        this.tube = tube;
    }

    private final JSEnv<E> env;
    private final Value tube;

    @NotNull
    @Override
    public TubeFormatVersion version() throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("version");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = TubeFormatVersion.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @NotNull
    @Override
    public Metadata metadata() throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("metadata");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = Metadata.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @Override
    public @NotNull FileSystemResource<E> getResource(@NotNull String id) throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("getResource");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        return UnwrapFileSystemResource.unwrap(env, res);
    }

    @NotNull
    @Override
    public ModuleDefinition getModule(@NotNull ModulePath id) throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("getModule");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = ModuleDefinition.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @NotNull
    @Override
    public ClassDefinition getClass(BigInteger id) throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("getClass");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = ClassDefinition.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @NotNull
    @Override
    public TraitDefinition getTrait(BigInteger id) throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("getTrait");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = TraitDefinition.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @NotNull
    @Override
    public FunctionDefinition getFunction(BigInteger id) throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("getFunction");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = FunctionDefinition.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @NotNull
    @Override
    public MethodDefinition getMethod(BigInteger id) throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("getMethod");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = MethodDefinition.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @NotNull
    @Override
    public ClassConstructorDefinition getClassConstructor(BigInteger id) throws E, IOException, InterruptedException {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("getClassConstructor");
        }
        finally {
            env.lock.unlock();
        }

        res = JavaFutureToJSPromise.runJSPromise(env, res);

        env.lock.lockInterruptibly();
        try {
            var builder = ClassConstructorDefinition.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }

    @Override
    public void close() throws Exception {
        Value res;
        env.lock.lockInterruptibly();
        try {
            res = tube.invokeMember("close");
        }
        finally {
            env.lock.unlock();
        }

        env.pluginOperations.catchAsRuntimeException(() -> JavaFutureToJSPromise.runJSPromise(env, res));
    }
}
