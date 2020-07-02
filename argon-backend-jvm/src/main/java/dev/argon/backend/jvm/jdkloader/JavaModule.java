package dev.argon.backend.jvm.jdkloader;

import org.jetbrains.annotations.NotNull;

import java.util.Set;

public abstract class JavaModule implements JavaLibrary {
    public abstract @NotNull String getModuleName();
    public abstract @NotNull Set<String> getRequires();

    @Override
    public final <T> T visit(@NotNull JavaLibraryVisitor<T> visitor) {
        return visitor.visitModule(this);
    }
}
