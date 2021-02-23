package dev.argon.backend.jvm.classmodule.jdkloader;

import dev.argon.util.ExIterable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.InputStream;

public interface JavaLibrary {

    // Class and package names have packages separated by /


    @NotNull ExIterable<String> packages();
    @NotNull ExIterable<String> classesInPackage(@NotNull String pkg) throws Exception;
    @Nullable InputStream findClass(@NotNull String name) throws Exception;

    <T> T visit(@NotNull JavaLibraryVisitor<T> visitor) throws Exception;
}
