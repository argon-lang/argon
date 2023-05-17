package dev.argon.plugin.adapter.js;

import org.graalvm.polyglot.Value;

public class JavaScriptException extends RuntimeException {
    public JavaScriptException(Value value) {

        this.value = value;
    }

    public final Value value;
}
