package dev.argon.vm;

import org.objectweb.asm.Type;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public abstract class FunctionDefinitionBase implements FunctionDefinition {
    public FunctionDefinitionBase(Tube tube, int functionNum, dev.argon.vm.format.FunctionDefinition.V1 functionDef) {
        this.tube = tube;
        this.functionNum = functionNum;
        this.functionDef = functionDef;
    }

    protected final Tube tube;
    protected final int functionNum;
    protected final dev.argon.vm.format.FunctionDefinition.V1 functionDef;

    @Override
    public final String className() {
        return "dev/argon/vm/app/tube" + tube.tubeNum() + "/Function" + functionNum;
    }

    @Override
    public final String methodName() {
        return "call";
    }

    @Override
    public final String descriptor() {
        var paramTypes = new Type[functionDef.parameterTypes().size()];
        for(int i = 0; i < paramTypes.length; ++i) {
            paramTypes[i] = TypeUtils.getType(functionDef.parameterTypes().get(i));
        }

        var returnType = TypeUtils.getType(functionDef.returnType());
        Type sig = Type.getMethodType(returnType, paramTypes);
        return sig.toString();
    }

    @Override
    public final boolean isInstanceMethod() {
        return false;
    }

    @Override
    public final boolean isInterface() {
        return false;
    }

    @Override
    @SuppressWarnings("nullness")
    public final void visitClass(ClassVisitor cv) {
        cv.visit(61, Opcodes.ACC_PUBLIC, className(), null, "java/lang/Object", null);
        var mv = cv.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, methodName(), descriptor(), null, null);
        visitMethod(mv);
        cv.visitEnd();
    }

    protected abstract void visitMethod(MethodVisitor mv);

}
