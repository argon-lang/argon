package dev.argon.vm;

import java.util.ArrayList;
import org.objectweb.asm.Type;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Label;
import org.checkerframework.checker.nullness.qual.Nullable;
import org.checkerframework.checker.nullness.qual.RequiresNonNull;
import dev.argon.vm.format.VMType;
import dev.argon.vm.format.Instruction;
import dev.argon.vm.format.JumpInstruction;

final class FunctionDefinitionImpl extends FunctionDefinitionBase implements FunctionDefinition {
    public FunctionDefinitionImpl(Runtime runtime, TubeImpl tube, int functionNum, dev.argon.vm.format.FunctionDefinition.V1 functionDef) {
        super(tube, functionNum, functionDef);
        this.runtime = runtime;
        this.tube = tube;
        
        if(functionDef.body().isPresent()) {
            body = functionDef.body().get();
        }
        else {
            body = null;
        }

        {
            localMap = new int[functionDef.parameterTypes().size() + body.localVariables().size()];
            int nextLocalIndex = 0;
            for(int i = 0; i < functionDef.parameterTypes().size(); ++i) {
                localMap[i] = nextLocalIndex;
                nextLocalIndex += switch(functionDef.parameterTypes().get(i)) {
                    case VMType.V1.I64 t -> 2;
                    case VMType.V1.F64 t -> 2;
                    default -> 1;
                };
            }
            for(int i = 0; i < body.localVariables().size(); ++i) {
                localMap[i + functionDef.parameterTypes().size()] = nextLocalIndex;
                nextLocalIndex += switch(body.localVariables().get(i)) {
                    case VMType.V1.I64 t -> 2;
                    case VMType.V1.F64 t -> 2;
                    default -> 1;
                };
            }
        }
    }

    private final Runtime runtime;
    private final TubeImpl tube;
    private final dev.argon.vm.format.FunctionBody.@Nullable V1 body;
    private final int[] localMap;

    @Override
    protected final void visitMethod(MethodVisitor mv) {
        visitParameters(mv);
        mv.visitCode();

        if(body != null) {
            var blocks = body.graph().blocks();
            var labels = new Label[blocks.size()];
            for(int i = 0; i < labels.length; ++i) {
                labels[i] = new Label();
            }

            for(int i = 0; i < blocks.size(); ++i) {
                if(i == 0) {
                    visitInitialFrame(mv);
                }
                else {
                    visitLaterFrame(mv);
                }

                mv.visitLabel(labels[i]);

                var block = blocks.get(i);
                for(int j = 0; j < block.instructions().size(); ++j) {
                    visitInstruction(mv, block.instructions().get(i));
                }
                visitJump(mv, labels, block.jump());
            }
        }
    }

    @SuppressWarnings("nullness")
    private void visitParameters(MethodVisitor mv) {
        for(int i = 0; i < functionDef.parameterTypes().size(); ++i) {
            mv.visitParameter(null, 0);
        }
    }

    @SuppressWarnings("nullness")
    @RequiresNonNull({"body"})
    private void visitInitialFrame(MethodVisitor mv) {
        var locals = new ArrayList<Object>();
        for(int i = 0; i < functionDef.parameterTypes().size(); ++i) {
            TypeUtils.getStackType(functionDef.parameterTypes().get(i), locals);
        }
        for(int i = 0; i < body.localVariables().size(); ++i) {
            TypeUtils.getStackType(body.localVariables().get(i), locals);
        }

        mv.visitFrame(Opcodes.F_FULL, locals.size(), locals.toArray(new Object[0]), 0, new Object[0]);
    }

    @SuppressWarnings("nullness")
    @RequiresNonNull({"body"})
    private void visitLaterFrame(MethodVisitor mv) {
        mv.visitFrame(Opcodes.F_SAME, 0, new Object[0], 0, new Object[0]);
    }

    @RequiresNonNull({"body"})
    private VMType.V1 getLocalType(dev.argon.vm.format.LocalId.V1 local) {
        int index = local.num().intValue();
        if(index < functionDef.parameterTypes().size()) {
            return functionDef.parameterTypes().get(index);
        }
        else {
            return body.localVariables().get(index - functionDef.parameterTypes().size());
        }
    }

    @RequiresNonNull({"body"})
    private void loadLocal(MethodVisitor mv, dev.argon.vm.format.LocalId.V1 local) {
        int index = localMap[local.num().intValue()];
        switch(getLocalType(local)) {
            case VMType.V1.I8 t -> mv.visitVarInsn(Opcodes.ILOAD, index);
            case VMType.V1.I16 t -> mv.visitVarInsn(Opcodes.ILOAD, index);
            case VMType.V1.I32 t -> mv.visitVarInsn(Opcodes.ILOAD, index);
            case VMType.V1.I64 t -> mv.visitVarInsn(Opcodes.LLOAD, index);
            case VMType.V1.F32 t -> mv.visitVarInsn(Opcodes.FLOAD, index);
            case VMType.V1.F64 t -> mv.visitVarInsn(Opcodes.DLOAD, index);
            default -> mv.visitVarInsn(Opcodes.ALOAD, index);
        }
    }

    @RequiresNonNull({"body"})
    private void storeLocal(MethodVisitor mv, dev.argon.vm.format.LocalId.V1 local) {
        int index = localMap[local.num().intValue()];
        switch(getLocalType(local)) {
            case VMType.V1.I8 t -> mv.visitVarInsn(Opcodes.ISTORE, index);
            case VMType.V1.I16 t -> mv.visitVarInsn(Opcodes.ISTORE, index);
            case VMType.V1.I32 t -> mv.visitVarInsn(Opcodes.ISTORE, index);
            case VMType.V1.I64 t -> mv.visitVarInsn(Opcodes.LSTORE, index);
            case VMType.V1.F32 t -> mv.visitVarInsn(Opcodes.FSTORE, index);
            case VMType.V1.F64 t -> mv.visitVarInsn(Opcodes.DSTORE, index);
            default -> mv.visitVarInsn(Opcodes.ASTORE, index);
        }
    }

    @SuppressWarnings("nullness")
    @RequiresNonNull({"body"})
    private void visitInstruction(MethodVisitor mv, Instruction.V1 insn) {
        switch(insn) {
            case Instruction.V1.ConstI constI -> {
                var value = constI.constI().value();
                var outVar = constI.constI().out();
                switch(getLocalType(outVar)) {
                    case VMType.V1.I8 u -> visitIntConstant(mv, value.byteValue());
                    case VMType.V1.I16 u -> visitIntConstant(mv, value.shortValue());
                    case VMType.V1.I32 u -> visitIntConstant(mv, value.intValue());
                    case VMType.V1.I64 u -> visitLongConstant(mv, value.longValue());
                    default -> throw new InvalidInstructionException();
                }
                storeLocal(mv, outVar);
            }

            case Instruction.V1.ConstInt constInt -> {
                throw new RuntimeException("Not implemented");
            }

            case Instruction.V1.ConstString constString -> {
                mv.visitLdcInsn(constString.constString().value());
                storeLocal(mv, constString.constString().out());
            }

            case Instruction.V1.ConstNull constNull -> {
                mv.visitInsn(Opcodes.ACONST_NULL);
                storeLocal(mv, constNull.constNull());
            }

            case Instruction.V1.ConvertSignExt conv -> {
                var in = conv.convertSignExt().in();
                var out = conv.convertSignExt().out();
                
                loadLocal(mv, in);
                
                if(getLocalType(out) instanceof VMType.V1.I64) {
                    mv.visitInsn(Opcodes.I2L);
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.ConvertZeroPad conv -> {
                var in = conv.convertZeroPad().in();
                var out = conv.convertZeroPad().out();
                
                loadLocal(mv, in);
                
                switch(getLocalType(in)) {
                    case VMType.V1.I8 t -> {
                        switch(getLocalType(out)) {
                            case VMType.V1.I16 u -> mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "toUnsignedInt", "(I)I", false);
                            case VMType.V1.I32 u -> mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "toUnsignedInt", "(I)I", false);
                            case VMType.V1.I64 u -> mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "toUnsignedLong", "(I)J", false);
                            default -> throw new InvalidInstructionException();
                        }
                    }

                    case VMType.V1.I16 t -> {
                        switch(getLocalType(out)) {
                            case VMType.V1.I32 u -> mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Short", "toUnsignedInt", "(I)I", false);
                            case VMType.V1.I64 u -> mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Short", "toUnsignedLong", "(I)J", false);
                            default -> throw new InvalidInstructionException();
                        }
                    }

                    case VMType.V1.I32 t -> {
                        switch(getLocalType(out)) {
                            case VMType.V1.I64 u -> mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Integer", "toUnsignedLong", "(I)J", false);
                            default -> throw new InvalidInstructionException();
                        }
                    }

                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.ConvertWrap conv -> {
                var in = conv.convertWrap().in();
                var out = conv.convertWrap().out();
                
                loadLocal(mv, in);
                
                switch(getLocalType(in)) {
                    case VMType.V1.I16 t -> {
                        switch(getLocalType(out)) {
                            case VMType.V1.I8 u -> mv.visitInsn(Opcodes.I2B);
                            default -> throw new InvalidInstructionException();
                        }
                    }

                    case VMType.V1.I32 t -> {
                        switch(getLocalType(out)) {
                            case VMType.V1.I8 u -> mv.visitInsn(Opcodes.I2B);
                            case VMType.V1.I16 u -> mv.visitInsn(Opcodes.I2S);
                            default -> throw new InvalidInstructionException();
                        }
                    }

                    case VMType.V1.I64 t -> {
                        switch(getLocalType(out)) {
                            case VMType.V1.I8 u -> {
                                mv.visitInsn(Opcodes.L2I);
                                mv.visitInsn(Opcodes.I2B);
                            }
                            case VMType.V1.I16 u -> {
                                mv.visitInsn(Opcodes.L2I);
                                mv.visitInsn(Opcodes.I2S);
                            }
                            case VMType.V1.I32 u -> mv.visitInsn(Opcodes.L2I);
                            default -> throw new InvalidInstructionException();
                        }
                    }

                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.Add op -> {
                var a = op.add().a();
                var b = op.add().b();
                var out = op.add().out();
                
                loadLocal(mv, a);
                loadLocal(mv, b);
                
                switch(getLocalType(a)) {
                    case VMType.V1.I8 t -> {
                        mv.visitInsn(Opcodes.IADD);
                        mv.visitInsn(Opcodes.I2B);
                    }

                    case VMType.V1.I16 t -> {
                        mv.visitInsn(Opcodes.IADD);
                        mv.visitInsn(Opcodes.I2S);
                    }

                    case VMType.V1.I32 t -> mv.visitInsn(Opcodes.IADD);
                    case VMType.V1.I64 t -> mv.visitInsn(Opcodes.LADD);
                    case VMType.V1.F32 t -> mv.visitInsn(Opcodes.FADD);
                    case VMType.V1.F64 t -> mv.visitInsn(Opcodes.DADD);
                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.Sub op -> {
                var a = op.sub().a();
                var b = op.sub().b();
                var out = op.sub().out();
                
                loadLocal(mv, a);
                loadLocal(mv, b);
                
                switch(getLocalType(a)) {
                    case VMType.V1.I8 t -> {
                        mv.visitInsn(Opcodes.ISUB);
                        mv.visitInsn(Opcodes.I2B);
                    }

                    case VMType.V1.I16 t -> {
                        mv.visitInsn(Opcodes.ISUB);
                        mv.visitInsn(Opcodes.I2S);
                    }

                    case VMType.V1.I32 t -> mv.visitInsn(Opcodes.ISUB);
                    case VMType.V1.I64 t -> mv.visitInsn(Opcodes.LSUB);
                    case VMType.V1.F32 t -> mv.visitInsn(Opcodes.FSUB);
                    case VMType.V1.F64 t -> mv.visitInsn(Opcodes.DSUB);
                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.Mul op -> {
                var a = op.mul().a();
                var b = op.mul().b();
                var out = op.mul().out();
                
                loadLocal(mv, a);
                loadLocal(mv, b);
                
                switch(getLocalType(a)) {
                    case VMType.V1.I8 t -> {
                        mv.visitInsn(Opcodes.IMUL);
                        mv.visitInsn(Opcodes.I2B);
                    }

                    case VMType.V1.I16 t -> {
                        mv.visitInsn(Opcodes.IMUL);
                        mv.visitInsn(Opcodes.I2S);
                    }

                    case VMType.V1.I32 t -> mv.visitInsn(Opcodes.IMUL);
                    case VMType.V1.I64 t -> mv.visitInsn(Opcodes.LMUL);
                    case VMType.V1.F32 t -> mv.visitInsn(Opcodes.FMUL);
                    case VMType.V1.F64 t -> mv.visitInsn(Opcodes.DMUL);
                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.Div op -> {
                var a = op.div().a();
                var b = op.div().b();
                var out = op.div().out();
                
                loadLocal(mv, a);
                loadLocal(mv, b);
                
                switch(getLocalType(a)) {
                    case VMType.V1.I8 t -> {
                        mv.visitInsn(Opcodes.IDIV);
                        mv.visitInsn(Opcodes.I2B);
                    }

                    case VMType.V1.I16 t -> {
                        mv.visitInsn(Opcodes.IDIV);
                        mv.visitInsn(Opcodes.I2S);
                    }

                    case VMType.V1.I32 t -> mv.visitInsn(Opcodes.IDIV);
                    case VMType.V1.I64 t -> mv.visitInsn(Opcodes.LDIV);
                    case VMType.V1.F32 t -> mv.visitInsn(Opcodes.FDIV);
                    case VMType.V1.F64 t -> mv.visitInsn(Opcodes.DDIV);
                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.DivU op -> {
                var a = op.divU().a();
                var b = op.divU().b();
                var out = op.divU().out();
                
                
                switch(getLocalType(a)) {
                    case VMType.V1.I8 t -> {
                        loadLocal(mv, a);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "toUnsignedInt", "(I)I", false);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "toUnsignedInt", "(I)I", false);
                        mv.visitInsn(Opcodes.IDIV);
                    }

                    case VMType.V1.I16 t -> {
                        loadLocal(mv, a);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Short", "toUnsignedInt", "(I)I", false);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Short", "toUnsignedInt", "(I)I", false);
                        mv.visitInsn(Opcodes.IDIV);
                    }

                    case VMType.V1.I32 t -> {
                        loadLocal(mv, a);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Integer", "divideUnsigned", "(II)I", false);
                    }

                    case VMType.V1.I64 t -> {
                        loadLocal(mv, a);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Long", "divideUnsigned", "(JJ)J", false);
                    }
                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.Rem op -> {
                var a = op.rem().a();
                var b = op.rem().b();
                var out = op.rem().out();
                
                loadLocal(mv, a);
                loadLocal(mv, b);
                
                switch(getLocalType(a)) {
                    case VMType.V1.I8 t -> {
                        mv.visitInsn(Opcodes.IREM);
                        mv.visitInsn(Opcodes.I2B);
                    }

                    case VMType.V1.I16 t -> {
                        mv.visitInsn(Opcodes.IREM);
                        mv.visitInsn(Opcodes.I2S);
                    }

                    case VMType.V1.I32 t -> mv.visitInsn(Opcodes.IREM);
                    case VMType.V1.I64 t -> mv.visitInsn(Opcodes.LREM);
                    case VMType.V1.F32 t -> mv.visitInsn(Opcodes.FREM);
                    case VMType.V1.F64 t -> mv.visitInsn(Opcodes.DREM);
                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.RemU op -> {
                var a = op.remU().a();
                var b = op.remU().b();
                var out = op.remU().out();


                switch(getLocalType(a)) {
                    case VMType.V1.I8 t -> {
                        loadLocal(mv, a);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "toUnsignedInt", "(I)I", false);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Byte", "toUnsignedInt", "(I)I", false);
                        mv.visitInsn(Opcodes.IREM);
                    }

                    case VMType.V1.I16 t -> {
                        loadLocal(mv, a);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Short", "toUnsignedInt", "(I)I", false);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Short", "toUnsignedInt", "(I)I", false);
                        mv.visitInsn(Opcodes.IREM);
                    }

                    case VMType.V1.I32 t -> {
                        loadLocal(mv, a);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Integer", "remainderUnsigned", "(II)I", false);
                    }

                    case VMType.V1.I64 t -> {
                        loadLocal(mv, a);
                        loadLocal(mv, b);
                        mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Long", "remainderUnsigned", "(JJ)J", false);
                    }
                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.Negate op -> {
                var in = op.negate().in();
                var out = op.negate().out();
                
                loadLocal(mv, in);
                
                switch(getLocalType(in)) {
                    case VMType.V1.I8 t -> {
                        mv.visitInsn(Opcodes.INEG);
                        mv.visitInsn(Opcodes.I2B);
                    }
                    case VMType.V1.I16 t -> {
                        mv.visitInsn(Opcodes.INEG);
                        mv.visitInsn(Opcodes.I2S);
                    }

                    case VMType.V1.I32 t -> mv.visitInsn(Opcodes.INEG);
                    case VMType.V1.I64 t -> mv.visitInsn(Opcodes.LNEG);
                    case VMType.V1.F32 t -> mv.visitInsn(Opcodes.FNEG);
                    case VMType.V1.F64 t -> mv.visitInsn(Opcodes.DNEG);

                    default -> throw new InvalidInstructionException();
                }

                storeLocal(mv, out);
            }

            case Instruction.V1.Call functionCall -> {
                var call = functionCall.call();
                var function = (FunctionDefinition)tube.getEntry(call.id().num().intValue());
                for(int i = 0; i < call.args().size(); ++i) {
                    loadLocal(mv, call.args().get(i));
                }
                
                int op = function.isInstanceMethod() ? Opcodes.INVOKEVIRTUAL : Opcodes.INVOKESTATIC;
                mv.visitMethodInsn(op, function.className(), function.methodName(), function.descriptor(), function.isInterface());
                storeLocal(mv, call.result());
            }

            default -> {
                throw new RuntimeException("Not implemented");
            }
        }
    }

    private void visitIntConstant(MethodVisitor mv, int value) {
        if(value == -1) {
            mv.visitInsn(Opcodes.ICONST_M1);
        }
        else if(value == 0) {
            mv.visitInsn(Opcodes.ICONST_0);
        }
        else if(value == 1) {
            mv.visitInsn(Opcodes.ICONST_1);
        }
        else if(value == 2) {
            mv.visitInsn(Opcodes.ICONST_2);
        }
        else if(value == 3) {
            mv.visitInsn(Opcodes.ICONST_3);
        }
        else if(value == 4) {
            mv.visitInsn(Opcodes.ICONST_4);
        }
        else if(value == 5) {
            mv.visitInsn(Opcodes.ICONST_5);
        }
        else if(value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE) {
            mv.visitIntInsn(Opcodes.BIPUSH, value);
        }
        else if(value >= Short.MIN_VALUE && value <= Short.MAX_VALUE) {
            mv.visitIntInsn(Opcodes.SIPUSH, value);
        }
        else {
            mv.visitLdcInsn(value);
        }
    }

    private void visitLongConstant(MethodVisitor mv, long value) {
        if(value == 0) {
            mv.visitInsn(Opcodes.LCONST_0);
        }
        else if(value == 1) {
            mv.visitInsn(Opcodes.LCONST_1);
        }
        else if(value >= Short.MIN_VALUE && value <= Short.MAX_VALUE) {
            visitIntConstant(mv, (int)value);
            mv.visitInsn(Opcodes.I2L);
        }
        else {
            mv.visitLdcInsn(value);
        }
    }

    @SuppressWarnings("nullness")
    @RequiresNonNull({"body"})
    private void visitJump(MethodVisitor mv, Label[] labels, JumpInstruction.V1 insn) {
        switch(insn) {
            case JumpInstruction.V1.Return ret -> {
                var in = ret._return().value();
                loadLocal(mv, in);
                mv.visitInsn(TypeUtils.getType(getLocalType(in)).getOpcode(Opcodes.IRETURN));
            }

            case JumpInstruction.V1.Jump jmp -> {
                var label = labels[jmp.jump().label().num().intValue()];
                mv.visitJumpInsn(Opcodes.GOTO, label);
            }

            case JumpInstruction.V1.JumpZero jmp -> {
                var value = jmp.jumpZero().value();
                var z = labels[jmp.jumpZero().zTarget().num().intValue()];
                var nz = labels[jmp.jumpZero().zTarget().num().intValue()];
                
                var t = getLocalType(value);
                if(t instanceof VMType.V1.I8 || t instanceof VMType.V1.I16 || t instanceof VMType.V1.I32) {
                    loadLocal(mv, value);
                    mv.visitJumpInsn(Opcodes.IFEQ, z);
                    mv.visitJumpInsn(Opcodes.GOTO, nz);
                }
                else if(t instanceof VMType.V1.I64) {
                    loadLocal(mv, value);
                    mv.visitInsn(Opcodes.LCONST_0);
                    mv.visitInsn(Opcodes.LCMP);
                    mv.visitJumpInsn(Opcodes.IFEQ, z);
                    mv.visitJumpInsn(Opcodes.GOTO, nz);
                }
                else if(t instanceof VMType.V1.F32) {
                    loadLocal(mv, value);
                    mv.visitInsn(Opcodes.FCONST_0);
                    mv.visitInsn(Opcodes.FCMPG);
                    mv.visitJumpInsn(Opcodes.IFEQ, z);
                    mv.visitJumpInsn(Opcodes.GOTO, nz);
                }
                else if(t instanceof VMType.V1.F64) {
                    loadLocal(mv, value);
                    mv.visitInsn(Opcodes.DCONST_0);
                    mv.visitInsn(Opcodes.DCMPG);
                    mv.visitJumpInsn(Opcodes.IFEQ, z);
                    mv.visitJumpInsn(Opcodes.GOTO, nz);
                }
                else {
                    loadLocal(mv, value);
                    mv.visitJumpInsn(Opcodes.IFNULL, z);
                    mv.visitJumpInsn(Opcodes.GOTO, nz);
                }
            }

            default -> throw new RuntimeException("Not implemented");
        }
    }
}
