package dev.argon.argonvm.engine;

import dev.argon.argonvm.StandardLibrary;

public class TestStandardLibrary extends StandardLibrary {

	public final StringBuilder output = new StringBuilder();

	@Override
	public void print(String value) throws Throwable {
		output.append(value);
	}
}
