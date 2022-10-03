package dev.argon.argonvm;

import org.jetbrains.annotations.NotNull;

public interface NativeFunctions {
	@NotNull VMFunction get(@NotNull  String name);
}
