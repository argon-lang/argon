package dev.argon.backend.codegen;

import dev.argon.vm.Instruction;
import dev.argon.vm.Region;

abstract class InstructionScanner {
	public final void scan(Region region) {
		switch (region) {
			case Region.BasicBlock basicBlock -> {
				for(var instruction : basicBlock.instructions()) {
					scan(instruction);
				}
			}
			case Region.Block block -> {
				scan(block.region());
			}
			case Region.Finally aFinally -> {
				scan(aFinally.action());
				scan(aFinally.ensuring());
			}
			case Region.IfElse ifElse -> {
				scan(ifElse.condition());
				scan(ifElse.whenTrue());
				scan(ifElse.whenFalse());
			}
			case Region.Sequence sequence -> {
				for(var subRegion : sequence.regions()) {
					scan(subRegion);
				}
			}
		}
	}

	public final void scan(Instruction instruction) {
		visitInstruction(instruction);
	}

	protected void visitInstruction(Instruction instruction) {
	}
}
