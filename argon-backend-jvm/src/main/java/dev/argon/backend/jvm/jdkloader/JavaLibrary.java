package dev.argon.backend.jvm.jdkloader;

import dev.argon.compiler.core.ModuleId;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;

public interface JavaLibrary {
    @Nullable InputStream findClass(@NotNull String pkg, @NotNull String name) throws IOException;

    <T> T visit(@NotNull JavaLibraryVisitor<T> visitor);


    public static @NotNull ModuleId idFromModuleName(@NotNull String name) {
        return new ModuleId("java:" +name);
    }

    public static final ModuleId classpathModuleId = new ModuleId("java!classpath");
}
