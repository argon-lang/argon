package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.ResourceFactory;
import dev.argon.plugin.api.ResourceRecorder;
import dev.argon.plugin.api.options.OptionDecoder;
import dev.argon.plugin.api.options.OptionDecodeException;
import dev.argon.plugin.api.tube.Toml;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

sealed class UnwrapOptionDecoder<E extends Throwable> implements OptionDecoder<E, Value> permits UnwrapOptionCodec {

    public UnwrapOptionDecoder(@NotNull JSEnv<E> env, @NotNull Value optionCodec) {

        this.env = env;
        this.optionCodec = optionCodec;
    }

    protected final JSEnv<E> env;
    protected final Value optionCodec;

    @Override
    public @NotNull Value decode(@NotNull ResourceFactory<E> resourceFactory, @NotNull Toml value) throws OptionDecodeException {
        try {
            env.lock.lockInterruptibly();
            try {
                var convValue = ProtoConverter.messageToJSProto(env, value);
                return WrapOptionDecodeResult.unwrap(env, optionCodec.invokeMember("decode", new WrapResourceFactory<>(env, resourceFactory), convValue));
            } finally {
                env.lock.unlock();
            }
        }
        catch(InterruptedException ex) {
            throw env.pluginOperations.wrapAsRuntimeException(ex);
        }
    }
}
