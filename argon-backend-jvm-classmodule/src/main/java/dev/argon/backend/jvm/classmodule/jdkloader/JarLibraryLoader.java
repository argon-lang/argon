package dev.argon.backend.jvm.classmodule.jdkloader;

import dev.argon.util.ExIterable;
import org.jetbrains.annotations.NotNull;
import scala.NotImplementedError;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public class JarLibraryLoader {
    private JarLibraryLoader() {}

    public static @NotNull JavaLibrary loadJar(@NotNull JarFile jar, @NotNull Runtime.Version jdkVersion) throws IOException {
        if(jdkVersion.feature() >= 9) {
            var moduleInfo = getModuleName(jar);
            return new JarModule(moduleInfo, jar);
        }
        else {
            return new JarClassPathEntry(jar);
        }
    }

    private static @NotNull ModuleInfoData getModuleName(@NotNull JarFile jar) throws IOException {

        var moduleInfoEntry = jar.getJarEntry("module-info.class");
        if(moduleInfoEntry != null) {
            try(var moduleInfoStream = jar.getInputStream(moduleInfoEntry)) {
                return ModuleInfoData.getModuleInfo(moduleInfoStream);
            }
        }


        var packages = searchModuleForPackages(jar);
        {
            var manifest = jar.getManifest();
            if(manifest != null) {
                String moduleName = manifest.getMainAttributes().getValue("Automatic-Module-Name");
                if(moduleName != null) {
                    return new ModuleInfoData(moduleName, packages, new HashSet<>());
                }
            }
        }

        return new ModuleInfoData(new File(jar.getName()).getName(), packages, new HashSet<>());
    }

    private static Set<String> searchModuleForPackages(JarFile jar) {
        var packages = new HashSet<String>();
        jar.stream()
            .filter(entry -> !shouldSkipEntry(entry))
            .forEach(entry -> packages.add(getPackage(entry.getName())));
        return packages;
    }

    private static String getPackage(String name) {
        int lastSlash = name.lastIndexOf('/');
        if(lastSlash < 0) return name;
        else return name.substring(0, lastSlash);
    }

    private static boolean shouldSkipEntry(JarEntry entry) {
        return entry.isDirectory() || entry.getName().startsWith("META-INF/") || !entry.getName().endsWith(".class");
    }

    private static InputStream findJarClass(JarFile jar, String name) throws IOException {
        var entryPath = name + ".class";
        var entry = jar.getJarEntry(entryPath);
        if(entry == null) return null;
        return jar.getInputStream(entry);
    }

    private static class JarModule extends JavaModule {

        public JarModule(ModuleInfoData moduleInfo, JarFile jar) {
            this.moduleInfo = moduleInfo;
            this.jar = jar;
        }

        private final ModuleInfoData moduleInfo;
        private final JarFile jar;

        @Override
        public @NotNull String getModuleName() {
            return moduleInfo.name;
        }

        @Override
        public @NotNull Set<String> getRequires() {
            return new HashSet<>(moduleInfo.packages);
        }

        @Override
        public boolean exportsPackage(String pkg) {
            return moduleInfo.packages.contains(pkg);
        }

        @Override
        public @NotNull ExIterable<String> packages() {
            return ExIterable.fromIterable(moduleInfo.packages);
        }

        @Override
        public @NotNull ExIterable<String> classesInPackage(@NotNull String pkg) throws Exception {
            return ExIterable.fromIterable(() ->
                jar.stream()
                    .filter(entry -> !shouldSkipEntry(entry) && moduleInfo.packages.contains(getPackage(entry.getName())))
                    .map(JarEntry::getName)
                    .iterator()
            );
        }

        @Override
        public InputStream findClass(String name) throws IOException {

            if(moduleInfo.packages == null || moduleInfo.packages.contains(getPackage(name))) {
                return findJarClass(jar, name);
            }
            else {
                return null;
            }
        }
    }

    private static class JarClassPathEntry extends ClassPath {

        public JarClassPathEntry(JarFile jar) {
            this.jar = jar;
        }

        private final JarFile jar;

        @Override
        public @NotNull ExIterable<String> packages() {
            throw new NotImplementedError();
        }

        @Override
        public @NotNull ExIterable<String> classesInPackage(@NotNull String pkg) throws Exception {
            throw new NotImplementedError();
        }

        @Override
        public InputStream findClass(String name) throws IOException {
            return findJarClass(jar, name);
        }
    }
}
