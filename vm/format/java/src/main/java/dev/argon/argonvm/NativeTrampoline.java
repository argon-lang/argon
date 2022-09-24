package dev.argon.argonvm;

import java.util.List;

public sealed interface NativeTrampoline {
	public static record Result(Object value) implements NativeTrampoline {}
	public static record DelayFunction(VMFunction next, Object[] args, Continuation1 continuation) implements NativeTrampoline {}

	@FunctionalInterface
	public static non-sealed interface Delay extends NativeTrampoline {
		NativeTrampoline invoke() throws Throwable;
	}

	@FunctionalInterface
	public static interface Continuation1 {
		NativeTrampoline invoke(Object arg) throws Throwable;
	}
}
