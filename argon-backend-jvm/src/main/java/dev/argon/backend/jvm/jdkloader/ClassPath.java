package dev.argon.backend.jvm.jdkloader;

import org.jetbrains.annotations.NotNull;

public abstract class ClassPath implements JavaLibrary {
    @Override
    public final <T> T visit(@NotNull JavaLibraryVisitor<T> visitor) {
        return visitor.visitClassPath(this);
    }
}
