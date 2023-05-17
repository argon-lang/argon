package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.PluginOperations;
import org.graalvm.polyglot.Context;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.Executor;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class JSEnv<E extends Throwable> {

    public JSEnv(@NotNull PluginOperations<E> pluginOperations, @NotNull Executor executor) {
        this.pluginOperations = pluginOperations;
        this.executor = executor;
    }
    
    public @NotNull PluginOperations<E> pluginOperations;
    
    public @NotNull Context context = Context.create("js");
    public @NotNull Lock lock = new ReentrantLock();

    public @NotNull Executor executor;

}
