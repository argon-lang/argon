package dev.argon.argonvm;

import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

public final class BytecodeVMFunction implements VMFunction {


	public BytecodeVMFunction(List<VMType> parameterTypes, VMType returnType, Chunk chunk) {
		this.parameterTypes = Collections.unmodifiableList(new ArrayList<>(parameterTypes));
		this.returnType = returnType;
		this.chunk = chunk;
	}

	private final List<VMType> parameterTypes;
	private final VMType returnType;
	private final Chunk chunk;

	@Override
	public List<VMType> parameterTypes() {
		return parameterTypes;
	}

	@Override
	public VMType returnType() {
		return returnType;
	}

	public Chunk chunk() {
		return chunk;
	}
}
