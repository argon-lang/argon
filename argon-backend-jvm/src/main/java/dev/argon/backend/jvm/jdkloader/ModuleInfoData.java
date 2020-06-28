package dev.argon.backend.jvm.jdkloader;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ModuleVisitor;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Set;
import static dev.argon.backend.jvm.jdkloader.Constants.*;

class ModuleInfoData {
    public ModuleInfoData(String name, Set<String> packages) {
        this.name = name;
        this.packages = packages;
    }


    public final String name;
    public final Set<String> packages;


    public static ModuleInfoData getModuleInfo(InputStream inputStream) throws IOException {

        final var visitor = new ClassVisitor(AsmVersion) {

            public String moduleName = null;
            public final Set<String> packages = new HashSet<>();

            @Override
            public ModuleVisitor visitModule(String name, int access, String version) {
                moduleName = name;

                return new ModuleVisitor(AsmVersion) {
                    @Override
                    public void visitExport(String packaze, int access, String... modules) {
                        if(modules == null) {
                            packages.add(packaze);
                        }
                    }
                };
            }
        };

        new ClassReader(inputStream).accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES);

        if(visitor.moduleName == null) {
            throw new InvalidModuleInfoException();
        }

        return new ModuleInfoData(visitor.moduleName, visitor.packages);
    }



}
