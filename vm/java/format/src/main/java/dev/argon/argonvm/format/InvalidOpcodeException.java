package dev.argon.argonvm.format;

public class InvalidOpcodeException extends VMFormatException {
	public InvalidOpcodeException(byte opcode) {
		super("Invalid opcode: " + Byte.toUnsignedInt(opcode));
	}
}
