package dev.argon.argonvm;

public abstract class VMException extends Exception {
	public VMException(String message) {
		super(message);
	}
}
