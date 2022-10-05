package dev.argon.argonvm.engine;

import dev.argon.argonvm.NativeTrampoline;

public sealed interface TrampolineFrame {
	public static record TrampolinedStackFrame(StackFrame frame) implements TrampolineFrame {}
	public static record Native(NativeTrampoline trampoline) implements TrampolineFrame {}
}
