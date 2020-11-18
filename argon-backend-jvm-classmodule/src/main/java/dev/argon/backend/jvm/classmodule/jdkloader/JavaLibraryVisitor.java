package dev.argon.backend.jvm.classmodule.jdkloader;

public interface JavaLibraryVisitor<T> {
    T visitModule(JavaModule module) throws Exception;
    T visitClassPath(ClassPath classPath) throws Exception;
}
