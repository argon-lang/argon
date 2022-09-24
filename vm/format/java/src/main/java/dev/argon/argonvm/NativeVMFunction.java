package dev.argon.argonvm;

public non-sealed interface NativeVMFunction extends VMFunction {
	NativeTrampoline invoke(Object... args) throws Throwable;
}
