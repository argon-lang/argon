package dev.argon.backend.jvm.jdkloader;

import dev.argon.compiler.core.ModuleId;

import java.io.IOException;
import java.io.InputStream;

public interface JavaLibrary {
    InputStream findClass(String pkg, String name) throws IOException;

    public static ModuleId idFromModuleName(String name) {
        return new ModuleId("java:" +name);
    }

    public static final ModuleId classpathModuleId = new ModuleId("java!classpath");
}
