package dev.argon.argonvm.engine;

import dev.argon.argonvm.NativeTrampoline;

public record NativeStackFrame(NativeTrampoline.Continuation1 continuation) implements CallStackFrame {
}
