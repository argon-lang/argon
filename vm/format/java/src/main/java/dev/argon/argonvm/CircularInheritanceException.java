package dev.argon.argonvm;

public class CircularInheritanceException extends VMFormatException {
	public CircularInheritanceException() {
		super("Circular inheritance");
	}
}
