package dev.argon.argonvm;

import java.util.List;

public sealed interface VMFunction permits BytecodeVMFunction, NativeVMFunction {
	List<VMType> parameterTypes();
	VMType returnType();
}
