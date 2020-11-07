package dev.argon.backend.jvm.classmodule.jdkloader;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;

public class JarLibraryLoader {
    private JarLibraryLoader() {}

    public static @NotNull JavaLibrary loadJar(@NotNull JarReader jar, @NotNull Runtime.Version jdkVersion) throws IOException {
        if(jdkVersion.feature() >= 9) {
            var moduleInfo = getModuleName(jar);
            return new JarModule(moduleInfo, jar);
        }
        else {
            return new JarClassPathEntry(jar);
        }
    }

    private static @NotNull ModuleInfoData getModuleName(@NotNull JarReader jar) throws IOException {

        try(var moduleInfoStream = jar.getEntryStream("module-info.class")) {
            if(moduleInfoStream != null) {
                return ModuleInfoData.getModuleInfo(moduleInfoStream);
            }
        }

        {
            var manifest = jar.getManifest();
            if(manifest != null) {
                String moduleName = manifest.getMainAttributes().getValue("Automatic-Module-Name");
                if(moduleName != null) {
                    return new ModuleInfoData(moduleName, null, new HashSet<>());
                }
            }
        }

        return new ModuleInfoData(jar.getJarName(), null, new HashSet<>());
    }

    private static InputStream findJarClass(JarReader jar, String pkg, String name) throws IOException {
        var entryPath = pkg.replace('.' , '/') + "/" + name + ".class";
        return jar.getEntryStream(entryPath);
    }

    private static class JarModule extends JavaModule {

        public JarModule(ModuleInfoData moduleInfo, JarReader jar) {
            this.moduleInfo = moduleInfo;
            this.jar = jar;
        }

        private final ModuleInfoData moduleInfo;
        private final JarReader jar;

        @Override
        public @NotNull String getModuleName() {
            return moduleInfo.name;
        }

        @Override
        public @NotNull Set<String> getRequires() {
            return new HashSet<>(moduleInfo.packages);
        }

        @Override
        public InputStream findClass(String pkg, String name) throws IOException {
            if(moduleInfo.packages == null || moduleInfo.packages.contains(pkg)) {
                return findJarClass(jar, pkg, name);
            }
            else {
                return null;
            }
        }
    }

    private static class JarClassPathEntry extends ClassPath {

        public JarClassPathEntry(JarReader jar) {
            this.jar = jar;
        }

        private final JarReader jar;

        @Override
        public InputStream findClass(String pkg, String name) throws IOException {
            return findJarClass(jar, pkg, name);
        }
    }
}
