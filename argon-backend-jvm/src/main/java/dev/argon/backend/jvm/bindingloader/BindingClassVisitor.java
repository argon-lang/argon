package dev.argon.backend.jvm.bindingloader;

import dev.argon.backend.jvm.Constants;
import org.objectweb.asm.*;
import org.objectweb.asm.signature.SignatureReader;

import java.io.IOException;
import java.io.InputStream;

import static org.objectweb.asm.Opcodes.*;

public class BindingClassVisitor extends ClassVisitor {

    public static BindingBuilder readClass(BindingBuilderFactory builderFactory, InputStream inputStream) throws IOException {
        var classReader = new ClassReader(inputStream);
        var visitor = new BindingClassVisitor(builderFactory);
        classReader.accept(visitor, ClassReader.SKIP_CODE);
        return visitor.bindingBuilder;
    }

    private BindingClassVisitor(BindingBuilderFactory builderFactory) {
        super(Constants.AsmVersion);
        this.builderFactory = builderFactory;
    }

    private final BindingBuilderFactory builderFactory;
    private BindingBuilder bindingBuilder = null;

    @Override
    public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
        bindingBuilder = builderFactory.create(access, name, signature, superName, interfaces);
    }

    @Override
    public void visitOuterClass(String owner, String name, String descriptor) {
        if(bindingBuilder != null) {
            bindingBuilder = bindingBuilder.withOuterClass(owner, name, descriptor);
        }
    }

    @Override
    public FieldVisitor visitField(int access, String name, String descriptor, String signature, Object value) {
        if(bindingBuilder != null && (access & (ACC_PUBLIC | ACC_PROTECTED)) != 0 && (access & ACC_SYNTHETIC) != ACC_SYNTHETIC) {
            var fieldBuilder = builderFactory.createFieldBuilder(access, name, descriptor, new SignatureReader(signature), value);
            bindingBuilder = bindingBuilder.withMethod(fieldBuilder.getter);
            if(fieldBuilder.setter != null) {
                bindingBuilder = bindingBuilder.withMethod(fieldBuilder.setter);
            }
        }

        return null;
    }

    @Override
    public MethodVisitor visitMethod(int access, String name, String descriptor, String signature, String[] exceptions) {
        if(bindingBuilder != null && (access & (ACC_PUBLIC | ACC_PROTECTED)) != 0 && (access & ACC_SYNTHETIC) != ACC_SYNTHETIC) {
            var methodBuilder = builderFactory.createMethodBuilder(access, name, descriptor, new SignatureReader(signature));
            bindingBuilder = bindingBuilder.withMethod(methodBuilder);

            return new BindingMethodVisitor(methodBuilder);
        }
        else {
            return null;
        }
    }
}
