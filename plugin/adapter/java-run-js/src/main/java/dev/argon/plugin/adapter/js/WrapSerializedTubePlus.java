package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.tube.SerializedTubePlus;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;

import java.math.BigInteger;

final class WrapSerializedTubePlus<E extends Throwable> extends WrapSerializedTube<E> {

    public WrapSerializedTubePlus(JSEnv<E> env, SerializedTubePlus<E, Value, Value, Value> tube) {
        super(env, tube);
        this.tube = tube;
    }
    
    private final SerializedTubePlus<E, Value, Value, Value> tube;

    @HostAccess.Export
    public @NotNull Value getExternMethodImplementation(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> tube.getExternMethodImplementation(id));
    }

    @HostAccess.Export
    public @NotNull Value getExternFunctionImplementation(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> tube.getExternFunctionImplementation(id));
    }

    @HostAccess.Export
    public @NotNull Value getExternClassConstructorImplementation(@NotNull BigInteger id) {
        return JavaFutureToJSPromise.executeAsPromise(env, () -> tube.getExternClassConstructorImplementation(id));
    }
}
