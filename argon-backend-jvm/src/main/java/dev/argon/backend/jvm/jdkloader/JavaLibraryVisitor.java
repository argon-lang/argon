package dev.argon.backend.jvm.jdkloader;

public interface JavaLibraryVisitor<T> {
    T visitModule(JavaModule module);
    T visitClassPath(ClassPath classPath);
}
