package dev.argon.backend.backends.js;

import org.graalvm.polyglot.Value;

@FunctionalInterface
public interface ThenCallback {
  void invoke(Value value);
}
