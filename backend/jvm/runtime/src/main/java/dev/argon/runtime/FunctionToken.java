package dev.argon.runtime;

public interface FunctionToken<B> {
	Trampoline<B> apply(Token token);
}
