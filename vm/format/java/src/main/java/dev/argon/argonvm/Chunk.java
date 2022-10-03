package dev.argon.argonvm;

import com.google.protobuf.ByteString;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public final class Chunk {
	public Chunk(@Nullable Object @NotNull [] constants, @NotNull VMType @NotNull[] variables, int[] labels, byte @NotNull[] bytecode) {
		this.constants = constants.clone();
		this.variables = variables.clone();
		this.labels = labels.clone();
		this.bytecode = bytecode.clone();
	}
	public Chunk(@Nullable Object @NotNull[] constants, @NotNull VMType @NotNull[] variables, int[] labels, @NotNull ByteString bytecode) {
		this.constants = constants.clone();
		this.variables = variables.clone();
		this.labels = labels.clone();
		this.bytecode = bytecode.toByteArray();
	}

	private final @Nullable Object @NotNull[] constants;
	private final @Nullable VMType @NotNull[] variables;
	private final int[] labels;
	private final byte @NotNull[] bytecode;

	public @Nullable Object getConstant(int index) {
		return constants[index];
	}

	public @NotNull List<@NotNull VMType> getVariables() {
		return List.of(variables);
	}

	public int getLabelTarget(int index) {
		return labels[index];
	}

	public @NotNull InstructionReader createReader() {
		return new InstructionReader(bytecode);
	}
}
