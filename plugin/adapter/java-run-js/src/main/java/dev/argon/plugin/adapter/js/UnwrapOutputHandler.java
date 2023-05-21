package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.FileSystemResource;
import dev.argon.plugin.api.options.OutputHandler;
import dev.argon.plugin.api.options.OutputInfo;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class UnwrapOutputHandler<E extends Throwable> implements OutputHandler<E, Value> {

    public UnwrapOutputHandler(JSEnv<E> env, Value outputHandler) {

        this.env = env;
        this.outputHandler = outputHandler;
    }

    private final JSEnv<E> env;
    private final Value outputHandler;

    @Override
    public @NotNull Map<@NotNull List<@NotNull String>, @NotNull OutputInfo<E, @NotNull Value>> options() {
        Map<List<String>, OutputInfo<E, Value>> res = new HashMap<>();

        try {
            env.lock.lockInterruptibly();
            try {
                outputHandler
                    .getMember("options")
                    .invokeMember("forEach", (MapForEachCallback)(key, outputInfo) -> res.put(decodeOptionKey(key), new UnwrapOutputInfo(outputInfo)));
            }
            finally {
                env.lock.unlock();
            }
        }
        catch(InterruptedException ex) {
            throw env.pluginOperations.wrapAsRuntimeException(ex);
        }

        return res;
    }

    public static interface MapForEachCallback {
        void apply(String key, Value outputInfo);
    }

    private static @NotNull List<@NotNull String> decodeOptionKey(@NotNull String key) {
        return Arrays.stream(key.split("\\.")).map(keyPart -> URLDecoder.decode(keyPart, StandardCharsets.UTF_8)).toList();
    }

    private class UnwrapOutputInfo implements OutputInfo<E, Value> {
        public UnwrapOutputInfo(Value outputInfo) {
            this.outputInfo = outputInfo;
        }
        private final Value outputInfo;

        @Override
        public @NotNull FileSystemResource<E> getValue(@NotNull Value output) {
            try {
                env.lock.lockInterruptibly();
                try {
                    return UnwrapFileSystemResource.unwrap(env, outputInfo.invokeMember("getValue", output));
                }
                finally {
                    env.lock.unlock();
                }
            }
            catch(InterruptedException ex) {
                throw env.pluginOperations.wrapAsRuntimeException(ex);
            }
        }
    }
}
