package dev.argon.backend.jvm.classmodule.jdkloader;

import org.jetbrains.annotations.NotNull;

public abstract class ClassPath implements JavaLibrary {

    public static final String classpathModuleId = "java!classpath";

    @Override
    public final <T> T visit(@NotNull JavaLibraryVisitor<T> visitor) throws Exception {
        return visitor.visitClassPath(this);
    }
}
