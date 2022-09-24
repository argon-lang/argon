package dev.argon.argonvm;

public class InvalidOpcodeException extends VMFormatException {
	public InvalidOpcodeException(byte opcode) {
		super("Invalid opcode: " + Byte.toUnsignedInt(opcode));
	}
}
