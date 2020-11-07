package dev.argon.backend.jvm.classmodule.loader;

import dev.argon.backend.jvm.classmodule.jdkloader.ClassPath;
import dev.argon.backend.jvm.classmodule.jdkloader.JavaLibrary;
import dev.argon.backend.jvm.classmodule.jdkloader.JavaLibraryVisitor;
import dev.argon.backend.jvm.classmodule.jdkloader.JavaModule;
import dev.argon.module.ModuleFormatVersion;
import dev.argon.module.java_format.*;
import dev.argon.util.ExIterable;
import dev.argon.util.ExIterator;
import org.jetbrains.annotations.NotNull;
import scala.NotImplementedError;

import java.util.*;

class JavaArModuleLoaderImpl implements JavaArModuleLoader {
    public JavaArModuleLoaderImpl(JavaArModuleLoaderFactory factory, JavaLibrary library) {
        this.factory = factory;
        this.library = library;
    }

    private final JavaArModuleLoaderFactory factory;
    private final JavaLibrary library;

    @Override
    public Metadata getMetadata() throws Exception {
        Metadata.newBuilder()
            .setFormatVersion(ModuleFormatVersion.currentVersion())
            .setName(library.visit(new LibraryNameVisitor()))
            .setModuleType(ModuleType.ReferenceModule);

        throw new NotImplementedError();
    }

    @Override
    public TraitDefinition getTraitDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public TraitReference getTraitRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassDefinition getClassDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassReference getClassRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public DataConstructorDefinition getDataCtorDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public DataConstructorReference getDataCtorRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public FunctionDefinition getFuncDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public FunctionReference getFuncRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public MethodDefinition getMethodDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public MethodReference getMethodRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassConstructorDefinition getClassCtorDef(int id) throws Exception {
        throw new NotImplementedError();
    }

    @Override
    public ClassConstructorReference getClassCtorRef(int id) throws Exception {
        throw new NotImplementedError();
    }

    private final class LibraryNameVisitor implements JavaLibraryVisitor<String> {
        @Override
        public String visitModule(JavaModule module) {
            return JavaModule.idFromModuleName(module.getModuleName());
        }

        @Override
        public String visitClassPath(ClassPath classPath) {
            return ClassPath.classpathModuleId;
        }
    }


    private final class LibraryReferencesVisitor implements JavaLibraryVisitor<ExIterable<ModuleReference>> {
        @Override
        public ExIterable<ModuleReference> visitModule(JavaModule module) {
            return new ExIterable<>() {
                @NotNull
                @Override
                public ExIterator<ModuleReference> iterator() {
                    final Set<String> seenModules = new HashSet<>();
                    final Queue<String> remainingModules = new ArrayDeque<>(module.getRequires());

                    return new ExIterator<>() {
                        @Override
                        public ModuleReference next() throws Exception {
                            String moduleName;
                            do {
                                moduleName = remainingModules.remove();
                            } while (seenModules.contains(moduleName));

                            var module = factory.loadByJavaModuleName(moduleName);

                            seenModules.add(moduleName);
                            return ModuleReference.newBuilder()
                                    .setName(moduleName)
                                    .build();
                        }

                        @Override
                        public void close() throws Exception {}
                    };
                }
            };
        }

        @Override
        public ExIterable<ModuleReference> visitClassPath(ClassPath classPath) {
            return ExIterable.empty();
        }
    }

    private final class LibraryDeclarationsCollector {

    }
}
