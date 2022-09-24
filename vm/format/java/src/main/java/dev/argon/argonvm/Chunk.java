package dev.argon.argonvm;

import com.google.protobuf.ByteString;
import java.util.List;

public final class Chunk {
	public Chunk(Object[] constants, VMType[] variables, byte[] bytecode) {
		this.constants = constants.clone();
		this.variables = variables.clone();
		this.bytecode = bytecode.clone();
	}
	public Chunk(Object[] constants, VMType[] variables, ByteString bytecode) {
		this.constants = constants.clone();
		this.variables = variables.clone();
		this.bytecode = bytecode.toByteArray();
	}

	private final Object[] constants;
	private final VMType[] variables;
	private final byte[] bytecode;

	public Object getConstant(int index) {
		return constants[index];
	}

	public List<VMType> getVariables() {
		return List.of(variables);
	}

	public InstructionReader createReader() {
		return new InstructionReader(bytecode);
	}
}
