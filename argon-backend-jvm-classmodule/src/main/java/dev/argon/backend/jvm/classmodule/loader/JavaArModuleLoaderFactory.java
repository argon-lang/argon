package dev.argon.backend.jvm.classmodule.loader;

import dev.argon.backend.jvm.classmodule.jdkloader.ClassPath;
import dev.argon.backend.jvm.classmodule.jdkloader.JavaModule;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;

public abstract class JavaArModuleLoaderFactory {
    public abstract @NotNull ArgonModuleSourceImpure loadClassPath() throws Exception;
    public abstract @NotNull ArgonModuleSourceImpure loadSystemModule(@NotNull String moduleName) throws Exception;
    public abstract @NotNull ArgonModuleSourceImpure loadJar(@NotNull Path jarFile) throws Exception;
    protected abstract @NotNull JavaModule loadByJavaModuleName(@NotNull String moduleName) throws Exception;

    public @NotNull ArgonModuleSourceImpure loadByArgonModuleId(@NotNull String moduleId) throws Exception {
        if(ClassPath.classpathModuleId.equals(moduleId)) {
            return loadClassPath();
        }
        else if(moduleId.startsWith(JavaModule.moduleIdPrefix)) {
            return new ArgonModuleSourceJava(this, loadByJavaModuleName(moduleId.substring(moduleId.length())));
        }
        else {
            throw new ModuleNotFoundException(moduleId);
        }
    }
}
