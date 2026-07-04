package dev.argon.runtime;

public interface Function<A, B> {
	Trampoline<B> apply(A a);
}
