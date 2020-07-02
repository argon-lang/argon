package dev.argon.backend.jvm.bindingloader;

import dev.argon.backend.jvm.Constants;
import org.objectweb.asm.MethodVisitor;

public class BindingMethodVisitor extends MethodVisitor {

    BindingMethodVisitor(MethodBuilder methodBuilder) {
        super(Constants.AsmVersion);
        this.methodBuilder = methodBuilder;
    }

    private MethodBuilder methodBuilder;



}
