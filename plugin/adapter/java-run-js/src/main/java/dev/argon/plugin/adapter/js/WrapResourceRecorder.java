package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.ResourceRecorder;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;

public class WrapResourceRecorder<E extends Throwable> {

    public WrapResourceRecorder(JSEnv<E> env, ResourceRecorder<E> recorder) {
        this.env = env;
        this.recorder = recorder;
    }

    private final JSEnv<E> env;
    private final ResourceRecorder<E> recorder;

    @HostAccess.Export
    public Value recordBinaryResource(Value resource) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            String res = recorder.recordBinaryResource(new UnwrapBinaryResource<>(env, resource));

            env.lock.lock();
            try {
                return env.context.asValue(res);
            }
            finally {
                env.lock.unlock();
            }
        });
    }

    @HostAccess.Export
    public Value recordDirectoryResource(Value resource) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> {
            String res = recorder.recordDirectoryResource(new UnwrapDirectoryResource<>(env, resource));

            env.lock.lock();
            try {
                return env.context.asValue(res);
            }
            finally {
                env.lock.unlock();
            }
        });
    }
}
