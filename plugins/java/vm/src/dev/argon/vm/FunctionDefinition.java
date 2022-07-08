package dev.argon.vm;

import org.objectweb.asm.Type;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public non-sealed interface FunctionDefinition extends ConstantPoolEntry {
    public String className();
    public String methodName();
    public String descriptor();
    public boolean isInstanceMethod();
    public boolean isInterface();

    public void visitClass(ClassVisitor cv);
}
