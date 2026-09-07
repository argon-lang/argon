package dev.argon.runtime;

public sealed interface Trampoline<T> {
	record Result<T>(T value) implements Trampoline<T> {
		public static <T> Result<T> of(T value) {
			return new Result<>(value);
		}
	}

	non-sealed interface Thunk<T> extends Trampoline<T> {
		Trampoline<T> step();
	}

	static <T> T resolve(Trampoline<T> value) {
		while (value instanceof Thunk<T> thunk) {
			value = thunk.step();
		}

		return ((Result<T>) value).value();
	}
}
