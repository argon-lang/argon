package dev.argon.driver.api.command;

import dev.argon.nobleidl.runtime.graaljsInterop.JSAdapter;
import dev.argon.nobleidl.runtime.graaljsInterop.JSExecutor;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.nio.file.Path;

public class CommandFilePath {
	private CommandFilePath() {}

	public static JSAdapter<Path> jsAdapter() {
		return new JSAdapter<>() {
			@Override
			public Path fromJS(Context context, JSExecutor jsExecutor, Value value) {
				return Path.of(value.asString());
			}

			@Override
			public Value toJS(Context context, JSExecutor jsExecutor, Path path) {
				return context.asValue(path.toString());
			}
		};
	}
}
