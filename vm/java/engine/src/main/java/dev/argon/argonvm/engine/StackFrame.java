package dev.argon.argonvm.engine;

import dev.argon.argonvm.Chunk;
import dev.argon.argonvm.InstructionReader;

import java.util.Stack;
import java.util.ArrayDeque;

final class StackFrame implements CallStackFrame {
	public StackFrame(Chunk chunk) {
		this.chunk = chunk;
		reader = chunk.createReader();

		var varTypes = chunk.getVariables();
		variables = new Object[varTypes.size()];
		for(int i = 0; i < variables.length; ++i) {
			variables[i] = varTypes.get(i).defaultValue();
		}
	}

	public StackFrame(Chunk chunk, Object[] argumentVariables) {
		this.chunk = chunk;
		reader = chunk.createReader();

		var varTypes = chunk.getVariables();
		variables = new Object[argumentVariables.length + varTypes.size()];
		for(int i = 0; i < argumentVariables.length; ++i) {
			variables[i] = argumentVariables[i];
		}
		for(int i = 0; i < varTypes.size(); ++i) {
			variables[i + argumentVariables.length] = varTypes.get(i).defaultValue();
		}
	}

	public final Chunk chunk;
	public final InstructionReader reader;
	public final Stack<Object> stack = new Stack<>();
	public Object[] variables;

}
