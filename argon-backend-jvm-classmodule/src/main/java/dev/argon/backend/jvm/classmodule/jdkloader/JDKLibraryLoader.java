package dev.argon.backend.jvm.classmodule.jdkloader;

import dev.argon.backend.jvm.classmodule.loader.InvalidModuleException;
import dev.argon.util.ExIterable;
import dev.argon.util.ExIterator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.*;
import java.util.*;
import java.util.stream.StreamSupport;

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

    private static @NotNull Path packageDir(@NotNull Path baseDir, @NotNull String pkg) throws Exception {
        if(pkg.isEmpty()) {
            return baseDir;
        }

        final var pkgParts = pkg.split("\\.");
        return packageDir(baseDir, Arrays.asList(pkgParts));
    }

    private static @NotNull Path packageDir(@NotNull Path baseDir, @NotNull Iterable<String> pkg) throws Exception {
        Path p = baseDir;
        for(var pkgPart : pkg) {
            if(pkgPart.contains(".")) throw new InvalidModuleException();
            p = p.resolve(pkgPart);
        }

        return p;
    }

    private static ExIterable<Path> listClassFiles(@NotNull Path packageDir) {
        return new ExIterable<Path>() {
            @Override
            public ExIterator<Path> iterator() throws Exception {
                if(!Files.exists(packageDir)) return ExIterable.<Path>empty().iterator();

                var stream = Files.newDirectoryStream(packageDir, "*.class");
                var iterator = stream.iterator();
                return new ExIterator<Path>() {
                    @Override
                    public @Nullable Path next() throws Exception {
                        if(iterator.hasNext()) return iterator.next();
                        else return null;
                    }

                    @Override
                    public void close() throws Exception {
                        stream.close();
                    }
                };
            }
        };
    }

    private static String appendToInternalName(String pkg, String name) {
        if(pkg.isEmpty()) return name;
        else return pkg + "/" + name;
    }

    private interface PackageSplitHandler<T> {
        T handleSplit(List<String> pkg, String className) throws Exception;
    }

    private static  <T> @Nullable T splitPackage(String name, PackageSplitHandler<T> handler) throws Exception {
        final var pkgParts = name.split("/");

        if(pkgParts[0].equals("META-INF")) {
            return null;
        }

        return handler.handleSplit(Arrays.asList(pkgParts).subList(0, pkgParts.length - 1), pkgParts[pkgParts.length - 1]);
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
        public boolean exportsPackage(String pkg) {
            return moduleInfo.packages.contains(pkg);
        }

        @Override
        public @NotNull Set<String> getRequires() {
            return new HashSet<>(moduleInfo.packages);
        }

        @Override
        public @NotNull ExIterable<String> packages() {
            return ExIterable.fromIterable(moduleInfo.packages);
        }

        @Override
        public @NotNull ExIterable<String> classesInPackage(@NotNull String pkg) throws Exception {
            return listClassFiles(packageDir(moduleDir, pkg)).map(path -> appendToInternalName(pkg, path.getFileName().toString()));
        }

        @Override
        public InputStream findClass(String name) throws Exception {
            return splitPackage(name, (pkg, className) -> {
                final var classFile = packageDir(moduleDir, pkg).resolve(className + ".class");

                if(!Files.exists(classFile)) {
                    return null;
                }

                return Files.newInputStream(classFile);
            });
        }
    }

    private static InputStream findCtSymClass(Collection<Path> classDirs, String name) throws Exception {
        return splitPackage(name, (pkg, className) -> {
            Path classFile = null;
            for(var classDir : classDirs) {
                final Path classFileCandidate = packageDir(classDir, pkg).resolve(className + ".sym");

                if(Files.exists(classFileCandidate)) {
                    classFile = classFileCandidate;
                    break;
                }
            }

            if(classFile == null) {
                return null;
            }

            return Files.newInputStream(classFile);
        });
    }

    private static ExIterable<String> findCtSymClassesInPackage(Collection<Path> classDirs, String pkg) {
        return ExIterable.fromIterable(classDirs)
            .flatMap(classDir -> listClassFiles(packageDir(classDir, pkg)))
            .map(path -> appendToInternalName(pkg, path.getFileName().toString()));
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
        public boolean exportsPackage(String pkg) {
            return moduleInfo.packages.contains(pkg);
        }

        @Override
        public @NotNull ExIterable<String> packages() {
            return ExIterable.fromIterable(moduleInfo.packages);
        }

        @Override
        public @NotNull ExIterable<String> classesInPackage(@NotNull String pkg) throws Exception {
            if(moduleInfo.packages.contains(pkg)) {
                return findCtSymClassesInPackage(classDirs, pkg);
            }
            else {
                return ExIterable.empty();
            }
        }

        @Override
        public InputStream findClass(String name) throws Exception {
            return splitPackage(name, (pkg, className) -> {
                String pkgStr = String.join("/", pkg);

                if(moduleInfo.packages.contains(pkgStr)) {
                    return findCtSymClass(classDirs, name);
                }
                else {
                    return null;
                }
            });
        }
    }

    private static final class PackageDirPair {
        private PackageDirPair(String packageName, Path path) {
            this.packageName = packageName;
            this.path = path;
        }

        public final String packageName;
        public final Path path;
    }

    private static final class CtSymClassPath extends ClassPath {

        public CtSymClassPath(List<Path> classDirs) {
            this.classDirs = classDirs;
        }

        private final List<Path> classDirs;

        @Override
        public @NotNull ExIterable<String> packages() {
            return new ExIterable<>() {
                @Override
                public ExIterator<String> iterator() throws Exception {
                    final Queue<PackageDirPair> packageDirs = new ArrayDeque<>();
                    for(var classDir : classDirs) {
                        packageDirs.add(new PackageDirPair("", classDir));
                    }

                    final Set<String> seenPackages = new HashSet<>();

                    return new ExIterator<>() {
                        private String currentPackageName = null;
                        private DirectoryStream<Path> currentDirStream = null;
                        private Iterator<Path> currentDirStreamIterator = null;

                        @Override
                        public @Nullable String next() throws Exception {
                            while(!packageDirs.isEmpty()) {
                                if(currentDirStream == null) {
                                    var packageDirPair = packageDirs.remove();

                                    currentDirStream = Files.newDirectoryStream(packageDirPair.path);
                                    currentPackageName = packageDirPair.packageName;
                                    currentDirStreamIterator = currentDirStream.iterator();
                                }

                                while(currentDirStreamIterator.hasNext()) {
                                    var entry = currentDirStreamIterator.next();
                                    String segment = entry.getFileName().toString();
                                    if(Files.isDirectory(entry)) {
                                        packageDirs.add(new PackageDirPair(appendToInternalName(currentPackageName, segment), entry));
                                    }
                                    else if(segment.endsWith(".class") && !seenPackages.contains(currentPackageName)) {
                                        seenPackages.add(currentPackageName);
                                        return currentPackageName;
                                    }
                                }

                                currentDirStreamIterator = null;
                                currentPackageName = null;
                                var prevDirStream = currentDirStream;
                                currentDirStream = null;
                                prevDirStream.close();
                            }

                            return null;
                        }

                        @Override
                        public void close() throws Exception {
                            currentDirStream.close();
                        }
                    };
                }
            };
        }

        @Override
        public @NotNull ExIterable<String> classesInPackage(@NotNull String pkg) throws Exception {
            return findCtSymClassesInPackage(classDirs, pkg);
        }

        @Override
        public InputStream findClass(String name) throws Exception {
            return findCtSymClass(classDirs, name);
        }
    }




}
