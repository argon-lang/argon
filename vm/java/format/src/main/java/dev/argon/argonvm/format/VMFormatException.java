package dev.argon.argonvm.format;

import dev.argon.argonvm.VMException;

public abstract class VMFormatException extends VMException {
	public VMFormatException(String message) {
		super(message);
	}
}
