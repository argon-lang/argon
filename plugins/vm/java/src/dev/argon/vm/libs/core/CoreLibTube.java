package dev.argon.vm.libs.core;

import dev.argon.vm.*;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Label;
import dev.argon.vm.format.VMType;
import java.util.Optional;

public abstract class CoreLibTube implements Tube {
    
    private static final VMType.V1 emptyTupleType = new VMType.V1.Tuple(
        dev.argon.verilization.runtime.List.<VMType.V1>fromSequence()
    );

    private static final String FUNCTION_RESULT = "dev/argon/vm/FunctionResult$Value";


    private class PrintFunction extends FunctionDefinitionBase {
        
        public PrintFunction() {
            super(CoreLibTube.this, 0, new dev.argon.vm.format.FunctionDefinition.V1(
                dev.argon.verilization.runtime.List.<VMType.V1>fromSequence(
                    new VMType.V1.Gcref(dev.argon.verilization.runtime.Unit.INSTANCE)
                ),
                emptyTupleType,
                Optional.<dev.argon.vm.format.FunctionBody.V1>empty()
            ));
        }

        @Override
        protected void visitMethod(MethodVisitor mv) {
            mv.visitParameter("s", 0);
            mv.visitCode();

            mv.visitFieldInsn(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;");
            mv.visitVarInsn(Opcodes.ALOAD, 0);
            mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/String;)V", false);
            
            var emptyTupleInternalName = TypeUtils.getType(emptyTupleType).getInternalName();
            mv.visitTypeInsn(Opcodes.NEW, emptyTupleInternalName);
            mv.visitInsn(Opcodes.DUP);
            mv.visitMethodInsn(Opcodes.INVOKESPECIAL, emptyTupleInternalName, "<init>", "()V", false);

            mv.visitTypeInsn(Opcodes.NEW, FUNCTION_RESULT);
            mv.visitInsn(Opcodes.DUP);
            mv.visitMethodInsn(Opcodes.INVOKESPECIAL, FUNCTION_RESULT, "<init>", "(Ljava/lang/Object;)V", false);
            mv.visitInsn(Opcodes.ARETURN);


            mv.visitEnd();
        }
    }

}
