package dev.argon.testrunner;

public class CommandFailureException extends Exception {
	public CommandFailureException(String message, String compilerOutput) {
		super(message);
		this.compilerOutput = compilerOutput;
	}

	private final String compilerOutput;

	public String getCompilerOutput() {
		return compilerOutput;
	}

	@Override
	public String toString() {
		return super.toString() + System.lineSeparator() + "Output:" + System.lineSeparator() + compilerOutput;
	}
}
