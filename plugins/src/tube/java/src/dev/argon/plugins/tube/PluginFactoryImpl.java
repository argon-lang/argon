package dev.argon.plugins.tube;

import dev.argon.plugin.api.*;

public class PluginFactoryImpl implements PluginFactory {
    @Override
    public <E extends Exception> Plugin<E, ?, ?> createPlugin() {
        return new PluginImpl<E>();
    }
}
