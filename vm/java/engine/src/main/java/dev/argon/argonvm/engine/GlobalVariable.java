package dev.argon.argonvm.engine;

import org.jetbrains.annotations.Nullable;

public class GlobalVariable {
	public GlobalVariable(@Nullable Object value) {
		this.value = value;
	}
	public @Nullable Object value;
}
