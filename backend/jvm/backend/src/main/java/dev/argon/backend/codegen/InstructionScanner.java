package dev.argon.backend.codegen;

import dev.argon.vm.Block;
import dev.argon.vm.Instruction;

abstract class InstructionScanner {
	public final void scan(Block block) {
		for(var instruction : block.instructions()) {
			scan(instruction);
		}
	}

	public final void scan(Instruction instruction) {
		visitInstruction(instruction);

		switch(instruction) {
			case Instruction.Block block ->
				scan(block.body());

			case Instruction.Finally finallyInsn -> {
				scan(finallyInsn.action());
				scan(finallyInsn.ensuring());
			}

			case Instruction.IfElse ifElse -> {
				scan(ifElse.condition());
				scan(ifElse.whenTrue());
				scan(ifElse.whenFalse());
			}

			default -> {
			}
		}
	}

	protected void visitInstruction(Instruction instruction) {
	}
}
