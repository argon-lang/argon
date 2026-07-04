package dev.argon.runtime;

public sealed interface Trampoline<T> {
	record Result<T>(T value) implements Trampoline<T> {}
	non-sealed interface Thunk<T> extends Trampoline<T> {
		Trampoline<T> step();
	}
}
