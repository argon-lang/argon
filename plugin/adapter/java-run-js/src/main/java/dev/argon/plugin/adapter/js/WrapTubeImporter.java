package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.TubeImporter;
import dev.argon.plugin.api.tube.TubeName;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class WrapTubeImporter<E extends Throwable> {

    public  WrapTubeImporter(JSEnv<E> env, @NotNull TubeImporter<E> tubeImporter) {
        this.env = env;
        this.tubeImporter = tubeImporter;
    }

    private final JSEnv<E> env;
    private final TubeImporter<E> tubeImporter;

    @HostAccess.Export
    @NotNull Value getTube(Value tubeName) throws E, IOException, InterruptedException {
        var nameBuilder = TubeName.newBuilder();
        env.lock.lockInterruptibly();
        try {
            ProtoConverter.buildFromJSProto(env, nameBuilder, tubeName);
        }
        finally {
            env.lock.unlock();
        }

        var res = tubeImporter.getTube(nameBuilder.build());
        var tube = new WrapSerializedTube<>(env, res);

        env.lock.lockInterruptibly();
        try {
            return env.context.asValue(tube);
        }
        finally {
            env.lock.unlock();
        }
    }

}
