package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.ResourceRecorder;
import dev.argon.plugin.api.options.OptionCodec;
import dev.argon.plugin.api.tube.Toml;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

final class UnwrapOptionCodec<E extends Throwable> extends UnwrapOptionDecoder<E> implements OptionCodec<E, Value> {

    public UnwrapOptionCodec(@NotNull JSEnv<E> env, @NotNull Value optionCodec) {
        super(env, optionCodec);
    }

    @Override
    public @NotNull Toml encode(@NotNull ResourceRecorder<E> recorder, @NotNull Value value) throws E, IOException, InterruptedException {
        env.lock.lockInterruptibly();
        try {
            var res = optionCodec.invokeMember("encode", new WrapResourceRecorder<>(env, recorder), value);
            var builder = Toml.newBuilder();
            ProtoConverter.buildFromJSProto(env, builder, res);
            return builder.build();
        }
        finally {
            env.lock.unlock();
        }
    }
}
