package dev.argon.backend.jvm.bindingloader;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class FieldBuilder {
    public FieldBuilder(@NotNull MethodBuilder getter, @Nullable MethodBuilder setter) {
        this.getter = getter;
        this.setter = setter;
    }

    public final MethodBuilder getter;
    public final MethodBuilder setter;
}
