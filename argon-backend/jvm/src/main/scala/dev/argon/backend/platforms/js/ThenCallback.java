package dev.argon.backend.platforms.js;

import org.graalvm.polyglot.Value;

@FunctionalInterface
public interface ThenCallback {
  void invoke(Value value);
}
