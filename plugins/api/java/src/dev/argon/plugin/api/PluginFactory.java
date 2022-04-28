package dev.argon.plugin.api;

public interface PluginFactory {
  <E extends Exception> Plugin<E, ?, ?> createPlugin();
}
