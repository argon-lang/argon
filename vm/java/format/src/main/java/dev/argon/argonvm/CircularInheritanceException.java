package dev.argon.argonvm;

import dev.argon.argonvm.format.VMFormatException;

public class CircularInheritanceException extends VMFormatException {
	public CircularInheritanceException() {
		super("Circular inheritance");
	}
}
