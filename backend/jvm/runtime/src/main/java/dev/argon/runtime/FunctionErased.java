package dev.argon.runtime;

public interface FunctionErased<B> {
	Trampoline<B> apply();
}
