package dev.argon.backend.jvm.classmodule.jdkloader;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class JDKLibraryLoader {
    private JDKLibraryLoader() {}

    public static @NotNull JavaLibraryIterator systemLibraries(@NotNull Runtime.Version jdkVersion) throws IOException {
        if(Runtime.version().feature() == jdkVersion.feature()) {
            return jrtLibs();
        }
        else {
            var javaHome = System.getProperty("java.home");
            if(javaHome == null) {
                return new JavaLibraryIterator() {
                    @Override
                    public JavaLibrary next() throws IOException {
                        return null;
                    }

                    @Override
                    public void close() throws Exception {
                    }
                };
            }

            return loadCtSymLibs(Paths.get(javaHome, "lib", "ct.sym"), jdkVersion);
        }
    }

    private static @NotNull JavaLibraryIterator jrtLibs() throws IOException {
        final var fs = FileSystems.newFileSystem(URI.create("jrt:/"), new HashMap<>());
        try {
            final var rootDir = fs.getRootDirectories().iterator().next();

            final var modulesDir = rootDir.resolve("modules");

            final var modulesDirStream = Files.newDirectoryStream(modulesDir);
            try {
                final var modulesDirIterator = modulesDirStream.iterator();

                return new JavaLibraryIterator() {

                    @Override
                    public JavaLibrary next() throws IOException {
                        if(!modulesDirIterator.hasNext()) {
                            return null;
                        }

                        final var moduleDir = modulesDirIterator.next();

                        ModuleInfoData moduleInfo;
                        try(var is = Files.newInputStream(moduleDir.resolve("module-info.sym"))) {
                            moduleInfo = ModuleInfoData.getModuleInfo(is);
                        }

                        return new JrtModule(moduleInfo, moduleDir);
                    }

                    @Override
                    public void close() throws Exception {
                        modulesDirStream.close();
                        fs.close();
                    }
                };
            }
            catch(Throwable ex) {
                modulesDirStream.close();
                throw ex;
            }
        }
        catch(Throwable ex) {
            fs.close();
            throw ex;
        }
    }

    private static @NotNull JavaLibraryIterator loadCtSymLibs(@NotNull Path ctSym, @NotNull Runtime.Version jdkVersion) throws IOException {
        final var fs = FileSystems.newFileSystem(ctSym, (ClassLoader)null);
        try {
            final var rootDir = fs.getRootDirectories().iterator().next();
            final var versionCode = jdkVersion.feature() < 10 ? Integer.toString(jdkVersion.feature()) : Character.toString('A' + (jdkVersion.feature() - 10));

            Path modulesDir = rootDir.resolve(versionCode + "-modules");
            if(!Files.isDirectory(modulesDir)) {
                modulesDir = null;
            }

            final List<Path> classDirs = new ArrayList<>();
            try(var classDirStream = Files.newDirectoryStream(rootDir)) {
                for(var p : classDirStream) {
                    if(p.getFileName().toString().contains("-")) {
                        continue;
                    }

                    if(!Files.isDirectory(p)) {
                        continue;
                    }

                    classDirs.add(p);
                }
            }

            if(classDirs.isEmpty()) {
                throw new MissingSystemLibraryException();
            }

            if(modulesDir == null) {
                return new JavaLibraryIterator() {

                    private JavaLibrary library = new CtSymClassPath(classDirs);

                    @Override
                    public JavaLibrary next() throws IOException {
                        try {
                            return library;
                        }
                        finally {
                            library = null;
                        }
                    }

                    @Override
                    public void close() throws Exception {
                        fs.close();
                    }
                };
            }
            else {
                final var moduleDirs = Files.newDirectoryStream(modulesDir);
                try {
                    final var moduleDirsIterator = moduleDirs.iterator();
                    return new JavaLibraryIterator() {
                        @Override
                        public JavaLibrary next() throws IOException, JDKLoaderException {
                            if(!moduleDirsIterator.hasNext()) {
                                return null;
                            }

                            final var moduleDir = moduleDirsIterator.next();

                            ModuleInfoData moduleInfo;
                            try(var is = Files.newInputStream(moduleDir.resolve("module-info.sym"))) {
                                moduleInfo = ModuleInfoData.getModuleInfo(is);
                            }

                            return new CtSymModule(moduleInfo, classDirs);
                        }

                        @Override
                        public void close() throws Exception {
                            moduleDirs.close();
                            fs.close();
                        }
                    };
                }
                catch(Throwable ex) {
                    moduleDirs.close();
                    throw ex;
                }
            }
        }
        catch(Throwable ex) {
            fs.close();
            throw ex;
        }
    }

    private static @NotNull Path packageDir(@NotNull Path baseDir, @NotNull String pkg) {
        if(pkg.isEmpty()) {
            return baseDir;
        }

        final var pkgParts = pkg.split("\\.");

        Path p = baseDir;
        for(var pkgPart : pkgParts) {
            p = p.resolve(pkgPart);
        }

        return p;
    }

    private static final class JrtModule extends JavaModule {

        public JrtModule(ModuleInfoData moduleInfo, Path moduleDir) {
            this.moduleInfo = moduleInfo;
            this.moduleDir = moduleDir;
        }

        private final ModuleInfoData moduleInfo;
        private final Path moduleDir;

        @Override
        public String getModuleName() {
            return moduleInfo.name;
        }

        @Override
        public @NotNull Set<String> getRequires() {
            return new HashSet<>(moduleInfo.packages);
        }

        @Override
        public InputStream findClass(String pkg, String name) throws IOException {
            final var pkgParts = pkg.split("\\.");

            if(pkgParts[0].equals("META-INF")) {
                return null;
            }
            else if(pkgParts[0].indexOf('-') >= 0) {
                return null;
            }

            final var classFile = packageDir(moduleDir, pkg).resolve(name + ".class");

            if(!Files.exists(classFile)) {
                return null;
            }

            return Files.newInputStream(classFile);
        }
    }

    private static InputStream findCtSymClass(Iterable<Path> classDirs, String pkg, String name) throws IOException {
        Path classFile = null;
        for(var classDir : classDirs) {
            final Path classFileCandidate = packageDir(classDir, pkg).resolve(name + ".sym");

            if(Files.exists(classFileCandidate)) {
                classFile = classFileCandidate;
                break;
            }
        }

        if(classFile == null) {
            return null;
        }

        return Files.newInputStream(classFile);
    }

    private static final class CtSymModule extends JavaModule {

        public CtSymModule(ModuleInfoData moduleInfo, List<Path> classDirs) {

            this.moduleInfo = moduleInfo;
            this.classDirs = classDirs;
        }

        private final ModuleInfoData moduleInfo;
        private final List<Path> classDirs;

        @Override
        public String getModuleName() {
            return moduleInfo.name;
        }

        @Override
        public @NotNull Set<String> getRequires() {
            return new HashSet<>(moduleInfo.packages);
        }

        @Override
        public InputStream findClass(String pkg, String name) throws IOException {
            if(moduleInfo.packages.contains(pkg)) {
                return findCtSymClass(classDirs, pkg, name);
            }
            else {
                return null;
            }
        }
    }

    private static final class CtSymClassPath extends ClassPath {

        public CtSymClassPath(List<Path> classDirs) {
            this.classDirs = classDirs;
        }

        private final List<Path> classDirs;

        @Override
        public InputStream findClass(String pkg, String name) throws IOException {
            return findCtSymClass(classDirs, pkg, name);
        }
    }




}
