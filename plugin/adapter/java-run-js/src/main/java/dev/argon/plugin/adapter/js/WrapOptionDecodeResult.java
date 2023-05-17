package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.options.OptionDecodeException;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

final class WrapOptionDecodeResult {
    private WrapOptionDecodeResult() {}


    public static @NotNull Value wrap(@NotNull JSEnv<?> env, @NotNull WrapOptionFunc func) {
        Value value;
        try {
            value = func.get();
        }
        catch(OptionDecodeException ex) {
            env.lock.lock();
            try {
                return env.context.eval("js", "errorMessage => ({ ok: false, errorMessage })").execute(ex.getMessage());
            }
            finally {
                env.lock.unlock();
            }
        }

        env.lock.lock();
        try {
            return env.context.eval("js", "value => ({ ok: true, value })").execute(value);
        }
        finally {
            env.lock.unlock();
        }
    }

    public static @NotNull Value unwrap(@NotNull JSEnv<?> env, @NotNull Value value) throws OptionDecodeException {
        env.lock.lock();
        try {
            if(value.getMember("ok").asBoolean()) {
                return value.getMember("value");
            }
            else {
                throw new OptionDecodeException(value.getMember("errorMessage").asString());
            }
        }
        finally {
            env.lock.unlock();
        }
    }

    @FunctionalInterface
    public static interface WrapOptionFunc {
        Value get() throws OptionDecodeException;
    }
}
