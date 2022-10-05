package dev.argon.argonvm;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public sealed interface NativeTrampoline {
	public static record Result(@Nullable Object value) implements NativeTrampoline {}
	public static record DelayFunction(@NotNull VMFunction next, @Nullable Object[] args, @NotNull Continuation1 continuation) implements NativeTrampoline {}

	@FunctionalInterface
	public static non-sealed interface Delay extends NativeTrampoline {
		NativeTrampoline invoke() throws Throwable;
	}

	@FunctionalInterface
	public static interface Continuation1 {
		@NotNull NativeTrampoline invoke(@Nullable Object arg) throws Throwable;
	}
}
