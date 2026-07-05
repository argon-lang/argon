package dev.argon.runtime;

public interface FunctionToken<A, B> {
	Trampoline<B> apply(A token);
}
