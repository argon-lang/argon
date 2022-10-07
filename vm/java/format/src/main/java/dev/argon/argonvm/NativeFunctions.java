package dev.argon.argonvm;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface NativeFunctions {
	@Nullable VMFunction get(@NotNull  String name);
}
