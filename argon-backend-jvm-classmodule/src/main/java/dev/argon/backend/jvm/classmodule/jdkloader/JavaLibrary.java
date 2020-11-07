package dev.argon.backend.jvm.classmodule.jdkloader;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;

public interface JavaLibrary {
    @Nullable InputStream findClass(@NotNull String pkg, @NotNull String name) throws IOException;

    <T> T visit(@NotNull JavaLibraryVisitor<T> visitor);
}
