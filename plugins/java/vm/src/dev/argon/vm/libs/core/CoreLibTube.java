package dev.argon.vm.libs.core;

import dev.argon.vm.*;
import dev.argon.vm.internal.*;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Label;
import dev.argon.vm.format.VMType;
import java.util.Optional;
import java.util.Map;
import java.util.HashMap;
import java.util.List;

public final class CoreLibTube implements Tube {
    
    public CoreLibTube(int tubeNum) {
        this.tubeNum = tubeNum;

        var printFunction = new PrintFunction();

        exports = new HashMap<>();
        exports.put(new Name(List.of("print")), printFunction);

        entries = new ConstantPoolEntry[] {
            printFunction,
        };
    }

    private final int tubeNum;
    private final Map<Name, ConstantPoolEntry> exports;
    private final ConstantPoolEntry[] entries;

    private static final VMType.V1 emptyTupleType = new VMType.V1.Tuple(
        dev.argon.verilization.runtime.List.<VMType.V1>fromSequence()
    );


    private static final Name tubeName = new Name(List.of("ArgonVM.CoreLib"));

    private static final String FUNCTION_RESULT = "dev/argon/vm/FunctionResult$Value";

    @Override
    public int tubeNum() {
        return tubeNum;
    }
    
    @Override
    public Name tubeName() {
        return tubeName;
    }

    @Override
    public ConstantPoolEntry getExport(Name name) throws Exception {
        var export = exports.get(name);
        if(export == null) {
            throw new UnknownExportException(tubeName, name);
        }
        return export;
    }
    
    @Override
    public ConstantPoolEntry getEntry(int id) {
        return entries[id];
    }


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
