package dev.argon.argonvm;

import dev.argon.argonvm.*;

import java.util.List;
import java.util.NoSuchElementException;

public abstract class StandardLibrary implements NativeFunctions {
	public void print(String value) throws Throwable {
		System.out.println(value);
	}

	public String int32_to_string(int value) throws Throwable {
		return Integer.toString(value);
	}

	public String uint32_to_string(int value) throws Throwable {
		return Integer.toUnsignedString(value);
	}

	public String float64_to_string(double value) throws Throwable {
		return Double.toString(value);
	}

	@Override
	public final VMFunction get(String name) {
		return switch(name) {
			case "print" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.ObjectReference());
				}

				@Override
				public VMType returnType() {
					return new VMType.Tuple(List.of());
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (String)args[0];
					print(value);
					return new NativeTrampoline.Result(new VMTuple(new Object[] {}));
				}
			};

			case "int32_to_string" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.Int32());
				}

				@Override
				public VMType returnType() {
					return new VMType.ObjectReference();
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (int)args[0];
					String result = int32_to_string(value);
					return new NativeTrampoline.Result(result);
				}
			};

			case "uint32_to_string" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.Int32());
				}

				@Override
				public VMType returnType() {
					return new VMType.ObjectReference();
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (int)args[0];
					String result = uint32_to_string(value);
					return new NativeTrampoline.Result(result);
				}
			};

			case "float64_to_string" -> new NativeVMFunction() {

				@Override
				public List<VMType> parameterTypes() {
					return List.of(new VMType.Float64());
				}

				@Override
				public VMType returnType() {
					return new VMType.ObjectReference();
				}

				@Override
				public NativeTrampoline invoke(Object... args) throws Throwable {
					var value = (double)args[0];
					String result = float64_to_string(value);
					return new NativeTrampoline.Result(result);
				}
			};

			default -> null;
		};
	}
}
