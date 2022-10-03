package dev.argon.argonvm.engine;

import dev.argon.argonvm.*;
import dev.argon.argonvm.format.Instruction;

import java.util.Stack;

public final class VM {
	public VM(Program program) {
		this.program = program;
	}

	private final Program program;
	private final Stack<CallStackFrame> callStack = new Stack<>();

	public void execute(Chunk chunk) throws Throwable {
		var frame = new StackFrame(chunk);
		run(frame);
	}

	private void run(StackFrame frame) throws Throwable {
		while(frame != null) {
			Instruction instruction = nextInstruction(frame);

			if(instruction instanceof Instruction.NOP) {
			}
			else if(instruction instanceof Instruction.POP) {
				frame.stack.pop();
			}
			else if(instruction instanceof Instruction.RETURN) {
				var result = frame.stack.pop();
				FrameExit frameExit = returnValue(result);
				if(frameExit instanceof FrameExit.ParentFrame pf) {
					frame = pf.frame();
				}
				else if(frameExit instanceof FrameExit.Result res) {
					frame = null;
				}
				else {
					throw new UnsupportedOperationException();
				}
			}
			else if(instruction instanceof Instruction.CONSTANT insn) {
				frame.stack.push(frame.chunk.getConstant((int) insn.index()));
			}
			else if(instruction instanceof Instruction.CONSTANT_0_INT32) {
				frame.stack.push(0);
			}
			else if(instruction instanceof Instruction.CONSTANT_1_INT32) {
				frame.stack.push(1);
			}
			else if(instruction instanceof Instruction.CONSTANT_NULL) {
				frame.stack.push(null);
			}
			else if(instruction instanceof Instruction.NEGATE) {
				new UnaryOpResOperand() {
					@Override
					public byte ofInt8(byte a) {
						return (byte) -a;
					}

					@Override
					public short ofInt16(short a) {
						return (short) -a;
					}

					@Override
					public int ofInt32(int a) {
						return -a;
					}

					@Override
					public long ofInt64(long a) {
						return -a;
					}

					@Override
					public float ofFloat32(float a) {
						return -a;
					}

					@Override
					public double ofFloat64(double a) {
						return -a;
					}

					@Override
					public VMTuple ofTuple(VMTuple a) {
						throw new UnsupportedOperationException();
					}

					@Override
					public Object ofObjectReference(Object a) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.ADD) {
				new BinaryOpResOperand() {
					@Override
					public byte ofInt8(byte a, byte b) {
						return (byte) (a + b);
					}

					@Override
					public short ofInt16(short a, short b) {
						return (short) (a + b);
					}

					@Override
					public int ofInt32(int a, int b) {
						return a + b;
					}

					@Override
					public long ofInt64(long a, long b) {
						return a + b;
					}

					@Override
					public float ofFloat32(float a, float b) {
						return a + b;
					}

					@Override
					public double ofFloat64(double a, double b) {
						return a + b;
					}

					@Override
					public VMTuple ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public Object ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.SUBTRACT) {
				new BinaryOpResOperand() {
					@Override
					public byte ofInt8(byte a, byte b) {
						return (byte) (a - b);
					}

					@Override
					public short ofInt16(short a, short b) {
						return (short) (a - b);
					}

					@Override
					public int ofInt32(int a, int b) {
						return a - b;
					}

					@Override
					public long ofInt64(long a, long b) {
						return a - b;
					}

					@Override
					public float ofFloat32(float a, float b) {
						return a - b;
					}

					@Override
					public double ofFloat64(double a, double b) {
						return a - b;
					}

					@Override
					public VMTuple ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public Object ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.MULTIPLY) {
				new BinaryOpResOperand() {
					@Override
					public byte ofInt8(byte a, byte b) {
						return (byte) (a * b);
					}

					@Override
					public short ofInt16(short a, short b) {
						return (short) (a * b);
					}

					@Override
					public int ofInt32(int a, int b) {
						return a * b;
					}

					@Override
					public long ofInt64(long a, long b) {
						return a * b;
					}

					@Override
					public float ofFloat32(float a, float b) {
						return a * b;
					}

					@Override
					public double ofFloat64(double a, double b) {
						return a * b;
					}

					@Override
					public VMTuple ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public Object ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.DIVIDE) {
				new BinaryOpResOperand() {
					@Override
					public byte ofInt8(byte a, byte b) {
						return (byte) (a / b);
					}

					@Override
					public short ofInt16(short a, short b) {
						return (short) (a / b);
					}

					@Override
					public int ofInt32(int a, int b) {
						return a / b;
					}

					@Override
					public long ofInt64(long a, long b) {
						return a / b;
					}

					@Override
					public float ofFloat32(float a, float b) {
						return a / b;
					}

					@Override
					public double ofFloat64(double a, double b) {
						return a / b;
					}

					@Override
					public VMTuple ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public Object ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.DIVIDE_UN) {
				new BinaryOpResOperand() {
					@Override
					public byte ofInt8(byte a, byte b) {
						return (byte) (Byte.toUnsignedInt(a) / Byte.toUnsignedInt(b));
					}

					@Override
					public short ofInt16(short a, short b) {
						return (short) (Short.toUnsignedInt(a) / Short.toUnsignedInt(b));
					}

					@Override
					public int ofInt32(int a, int b) {
						return Integer.divideUnsigned(a, b);
					}

					@Override
					public long ofInt64(long a, long b) {
						return Long.divideUnsigned(a, b);
					}

					@Override
					public float ofFloat32(float a, float b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public double ofFloat64(double a, double b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public VMTuple ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public Object ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.EQZ) {
				var a = frame.stack.pop();
				frame.stack.push(valueIsZero(a) ? 1 : 0);
			}
			else if(instruction instanceof Instruction.NEZ) {
				var a = frame.stack.pop();
				frame.stack.push(valueIsZero(a) ? 0 : 1);
			}
			else if(instruction instanceof Instruction.EQ) {
				var b = frame.stack.pop();
				var a = frame.stack.pop();
				frame.stack.push(valuesAreEqual(a, b) ? 1 : 0);
			}
			else if(instruction instanceof Instruction.NE) {
				var b = frame.stack.pop();
				var a = frame.stack.pop();
				frame.stack.push(valuesAreEqual(a, b) ? 0 : 1);
			}
			else if(instruction instanceof Instruction.LT) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return a < b;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return a < b;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return a < b;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return a < b;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return a < b;
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return a < b;
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.LT_UN) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return Byte.compareUnsigned(a, b) < 0;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return Short.compareUnsigned(a, b) < 0;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return Integer.compareUnsigned(a, b) < 0;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return Long.compareUnsigned(a, b) < 0;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return !(a >= b);
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return !(a >= b);
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.LE) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return a <= b;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return a <= b;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return a <= b;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return a <= b;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return a <= b;
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return a <= b;
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.LE_UN) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return Byte.compareUnsigned(a, b) <= 0;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return Short.compareUnsigned(a, b) <= 0;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return Integer.compareUnsigned(a, b) <= 0;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return Long.compareUnsigned(a, b) <= 0;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return !(a > b);
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return !(a > b);
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.GT) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return a > b;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return a > b;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return a > b;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return a > b;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return a > b;
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return a > b;
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.GT_UN) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return Byte.compareUnsigned(a, b) > 0;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return Short.compareUnsigned(a, b) > 0;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return Integer.compareUnsigned(a, b) > 0;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return Long.compareUnsigned(a, b) > 0;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return !(a <= b);
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return !(a <= b);
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.GE) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return a >= b;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return a >= b;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return a >= b;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return a >= b;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return a >= b;
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return a >= b;
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.GE_UN) {
				new BinaryOpResBool() {
					@Override
					public boolean ofInt8(byte a, byte b) {
						return Byte.compareUnsigned(a, b) >= 0;
					}

					@Override
					public boolean ofInt16(short a, short b) {
						return Short.compareUnsigned(a, b) >= 0;
					}

					@Override
					public boolean ofInt32(int a, int b) {
						return Integer.compareUnsigned(a, b) >= 0;
					}

					@Override
					public boolean ofInt64(long a, long b) {
						return Long.compareUnsigned(a, b) >= 0;
					}

					@Override
					public boolean ofFloat32(float a, float b) {
						return !(a < b);
					}

					@Override
					public boolean ofFloat64(double a, double b) {
						return !(a < b);
					}

					@Override
					public boolean ofTuple(VMTuple a, VMTuple b) {
						throw new UnsupportedOperationException();
					}

					@Override
					public boolean ofObjectReference(Object a, Object b) {
						throw new UnsupportedOperationException();
					}
				}.execute(frame);
			}
			else if(instruction instanceof Instruction.JMP insn) {
				frame.reader.jump(frame.chunk.getLabelTarget((int)insn.label()));
			}
			else if(instruction instanceof Instruction.JZ insn) {
				var a = frame.stack.pop();
				if(valueIsZero(a)) {
					frame.reader.jump(frame.chunk.getLabelTarget((int)insn.label()));
				}
			}
			else if(instruction instanceof Instruction.JNZ insn) {
				var a = frame.stack.pop();
				if(!valueIsZero(a)) {
					frame.reader.jump(frame.chunk.getLabelTarget((int)insn.label()));
				}
			}
			else if(instruction instanceof Instruction.CALL insn) {
				frame = callImpl(frame, insn.index(), false);
			}
			else if(instruction instanceof Instruction.RETURN_CALL insn) {
				frame = callImpl(frame, insn.index(), true);
			}
			else if(instruction instanceof Instruction.LD_LOCAL insn) {
				frame.stack.push(frame.variables[(int) insn.index()]);
			}
			else if(instruction instanceof Instruction.ST_LOCAL insn) {
				frame.variables[(int) insn.index()] = frame.stack.pop();
			}
			else if(instruction instanceof Instruction.NEW insn) {
				VMClass cls = program.getClass(insn.class_index());
				frame.stack.push(new VMObject(program, cls));
			}
			else if(instruction instanceof Instruction.ST_FIELD insn) {
				var fieldId = new FieldId(insn.class_index(), insn.field_index());
				var value = frame.stack.pop();
				var obj = (VMObject)frame.stack.pop();
				obj.storeField(fieldId, value);
			}
			else if(instruction instanceof Instruction.LD_FIELD insn) {
				var fieldId = new FieldId(insn.class_index(), insn.field_index());
				var obj = (VMObject)frame.stack.pop();
				var value = obj.loadField(fieldId);
				frame.stack.push(value);
			}
			else if(instruction instanceof Instruction.CALL_CLASS insn) {
				VMClass cls = program.getClass(insn.class_index());
				var sig = cls.slots().get((int)insn.slot_index());
				var thisValue = (VMObject)frame.stack.elementAt(frame.stack.size() - sig.parameterTypes().size());
				var functionId = thisValue.vmClass().getImplementationFunctionId(new SlotKey(insn.class_index(), insn.slot_index()));
				frame = callImpl(frame, functionId, false);
			}
			else {
				throw new UnsupportedOperationException("Unknown instruction: " + instruction);
			}
		}
	}

	private Instruction nextInstruction(StackFrame frame) throws Throwable {
		return Instruction.read(frame.reader);
	}

	private FrameExit returnValue(Object value) throws Throwable {
		TrampolinedFrameExit result = returnValueTramp(value);
		FrameExit fe = null;
		while(fe == null) {
			if(result instanceof FrameExit fe2) {
				fe = fe2;
			}
			else if(result instanceof FrameExitDelay delay) {
				result = runTrampolineStep(new TrampolineFrame.Native(delay.trampoline()));
			}
			else {
				throw new UnsupportedOperationException();
			}
		}
		return fe;
	}

	private TrampolinedFrameExit returnValueTramp(Object value) throws Throwable {
		if(callStack.empty()) {
			return new FrameExit.Result(value);
		}
		else {
			CallStackFrame pop = callStack.pop();
			if(pop instanceof StackFrame frame) {
				frame.stack.push(value);
				return new FrameExit.ParentFrame(frame);
			} else if(pop instanceof NativeStackFrame frame) {
				return new FrameExitDelay(frame.continuation().invoke(value));
			}
			else {
				throw new UnsupportedOperationException();
			}
		}
	}

	private TrampolinedFrameExit runTrampolineStep(TrampolineFrame frame) throws Throwable {
		TrampolinedFrameExit result = null;
		while(result == null) {
			if(frame instanceof TrampolineFrame.TrampolinedStackFrame frame2) {
				result = new FrameExit.ParentFrame(frame2.frame());
			}
			else if(frame instanceof TrampolineFrame.Native nativeFrame) {
				if(nativeFrame.trampoline() instanceof NativeTrampoline.Result nativeRes) {
					result = returnValueTramp(nativeRes.value());
				}
				else if(nativeFrame.trampoline() instanceof NativeTrampoline.DelayFunction delayFunction) {
					callStack.push(new NativeStackFrame(delayFunction.continuation()));
					frame = callFunction(delayFunction.next(), delayFunction.args());
				}
				else if(nativeFrame.trampoline() instanceof NativeTrampoline.Delay delay) {
					frame = new TrampolineFrame.Native(delay.invoke());
				}
				else {
					throw new UnsupportedOperationException();
				}
			}
		}
		return result;
	}

	// Returns null if the program should exit.
	private StackFrame callImpl(StackFrame frame, long functionId, boolean dropCurrentFrame) throws Throwable {
		var function = program.getFunction(functionId);
		var args = new Object[function.parameterTypes().size()];
		for(int i = args.length - 1; i >= 0; --i) {
			args[i] = frame.stack.pop();
		}

		if(!dropCurrentFrame) {
			callStack.push(frame);
		}

		var trampoline = callFunction(function, args);
		FrameExit frameExit = runTrampoline(trampoline);
		if(frameExit instanceof FrameExit.Result res) {
			return null;
		}
		else if(frameExit instanceof FrameExit.ParentFrame parentFrame) {
			return parentFrame.frame();
		}
		else {
			throw new UnsupportedOperationException();
		}
	}

	private TrampolineFrame callFunction(VMFunction function, Object[] args) throws Throwable {
		if(function instanceof BytecodeVMFunction f) {
			return new TrampolineFrame.TrampolinedStackFrame(new StackFrame(f.chunk(), args));
		}
		else if(function instanceof NativeVMFunction f) {
			return new TrampolineFrame.Native(f.invoke(args));
		}
		else {
			throw new UnsupportedOperationException();
		}
	}

	private FrameExit runTrampoline(TrampolineFrame frame) throws Throwable {
		var step = runTrampolineStep(frame);
		FrameExit result = null;
		while(result == null) {
			if(step instanceof FrameExit exit) {
				result = exit;
			}
			else if(step instanceof FrameExitDelay delay) {
				step = runTrampolineStep(new TrampolineFrame.Native(delay.trampoline()));
			}
			else {
				throw new UnsupportedOperationException();
			}
		}
		return result;
	}

	private boolean valuesAreEqual(Object a, Object b) {
		if(a instanceof Byte ax) {
			return ax == (byte)b;
		}
		else if(a instanceof Short ax) {
			return ax == (short)b;
		}
		else if(a instanceof Integer ax) {
			return ax == (int)b;
		}
		else if(a instanceof Long ax) {
			return ax == (Long)b;
		}
		else if(a instanceof Float ax) {
			return ax == (float)b;
		}
		else if(a instanceof Double ax) {
			return ax == (double)b;
		}
		else if(a instanceof VMTuple ax) {
			var bx = (VMTuple)b;
			if(ax.size() != bx.size()) {
				return false;
			}
			for(int i = 0; i < ax.size(); ++i) {
				if(ax.get(i) != bx.get(i)) {
					return false;
				}
			}
			return true;
		}
		else if(
			b instanceof Byte || b instanceof Short ||
				b instanceof Integer || b instanceof Long ||
				b instanceof Float || b instanceof Double ||
				b instanceof VMTuple
		) {
			throw new UnsupportedOperationException();
		}
		else {
			return a == b;
		}
	}

	private boolean valueIsZero(Object a) {
		if(a instanceof Byte ax) {
			return ax == 0;
		}
		else if(a instanceof Short ax) {
			return ax == 0;
		}
		else if(a instanceof Integer ax) {
			return ax == 0;
		}
		else if(a instanceof Long ax) {
			return ax == 0;
		}
		else if(a instanceof Float ax) {
			return ax == 0;
		}
		else if(a instanceof Double ax) {
			return ax == 0;
		}
		else if(a instanceof VMTuple ax) {
			throw new UnsupportedOperationException();
		}
		else {
			return a == null;
		}
	}

	private abstract class UnaryOpResOperand {
		public abstract byte ofInt8(byte a);
		public abstract short ofInt16(short a);
		public abstract int ofInt32(int a);
		public abstract long ofInt64(long a);
		public abstract float ofFloat32(float a);
		public abstract double ofFloat64(double a);
		public abstract VMTuple ofTuple(VMTuple a);
		public abstract Object ofObjectReference(Object a);

		public final void execute(StackFrame frame) {
			Object a = frame.stack.pop();
			Object res;
			if(a instanceof Byte ax) {
				res = ofInt8(ax);
			}
			else if(a instanceof Short ax) {
				res = ofInt16(ax);
			}
			else if(a instanceof Integer ax) {
				res = ofInt32(ax);
			}
			else if(a instanceof Long ax) {
				res = ofInt64(ax);
			}
			else if(a instanceof Float ax) {
				res = ofFloat32(ax);
			}
			else if(a instanceof Double ax) {
				res = ofFloat64(ax);
			}
			else if(a instanceof VMTuple ax) {
				res = ofTuple(ax);
			}
			else {
				res = ofObjectReference(a);
			}
			frame.stack.push(res);
		}
	}

	private abstract class BinaryOpResOperand {
		public abstract byte ofInt8(byte a, byte b);
		public abstract short ofInt16(short a, short b);
		public abstract int ofInt32(int a, int b);
		public abstract long ofInt64(long a, long b);
		public abstract float ofFloat32(float a, float b);
		public abstract double ofFloat64(double a, double b);
		public abstract VMTuple ofTuple(VMTuple a, VMTuple b);
		public abstract Object ofObjectReference(Object a, Object b);

		public final void execute(StackFrame frame) {
			Object b = frame.stack.pop();
			Object a = frame.stack.pop();
			Object res;
			if(a instanceof Byte ax) {
				res = ofInt8(ax, (byte) b);
			}
			else if(a instanceof Short ax) {
				res = ofInt16(ax, (short) b);
			}
			else if(a instanceof Integer ax) {
				res = ofInt32(ax, (int) b);
			}
			else if(a instanceof Long ax) {
				res = ofInt64(ax, (long) b);
			}
			else if(a instanceof Float ax) {
				res = ofFloat32(ax, (float) b);
			}
			else if(a instanceof Double ax) {
				res = ofFloat64(ax, (double) b);
			}
			else if(a instanceof VMTuple ax) {
				res = ofTuple(ax, (VMTuple) b);
			}
			else {
				if(
					b instanceof Byte || b instanceof Short ||
						b instanceof Integer || b instanceof Long ||
						b instanceof Float || b instanceof Double ||
						b instanceof VMTuple
				) {
					throw new UnsupportedOperationException();
				}

				res = ofObjectReference(a, b);
			}

			frame.stack.push(res);
		}
	}

	private abstract class BinaryOpResBool {
		public abstract boolean ofInt8(byte a, byte b);
		public abstract boolean ofInt16(short a, short b);
		public abstract boolean ofInt32(int a, int b);
		public abstract boolean ofInt64(long a, long b);
		public abstract boolean ofFloat32(float a, float b);
		public abstract boolean ofFloat64(double a, double b);
		public abstract boolean ofTuple(VMTuple a, VMTuple b);
		public abstract boolean ofObjectReference(Object a, Object b);

		public final void execute(StackFrame frame) {
			Object b = frame.stack.pop();
			Object a = frame.stack.pop();
			boolean res;
			if(a instanceof Byte ax) {
				res = ofInt8(ax, (byte) b);
			}
			else if(a instanceof Short ax) {
				res = ofInt16(ax, (short) b);
			}
			else if(a instanceof Integer ax) {
				res = ofInt32(ax, (int) b);
			}
			else if(a instanceof Long ax) {
				res = ofInt64(ax, (long) b);
			}
			else if(a instanceof Float ax) {
				res = ofFloat32(ax, (float) b);
			}
			else if(a instanceof Double ax) {
				res = ofFloat64(ax, (double) b);
			}
			else if(a instanceof VMTuple ax) {
				res = ofTuple(ax, (VMTuple) b);
			}
			else {
				if(
					b instanceof Byte || b instanceof Short ||
						b instanceof Integer || b instanceof Long ||
						b instanceof Float || b instanceof Double ||
						b instanceof VMTuple
				) {
					throw new UnsupportedOperationException();
				}

				res = ofObjectReference(a, b);
			}
			frame.stack.push(res);
		}
	}
}
